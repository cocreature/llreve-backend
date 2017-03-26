{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Sem
import           Control.Exception (IOException)
import           Control.Monad.Catch hiding (Handler)
import           Control.Monad.Except
import qualified Control.Monad.Log as Log
import           Control.Monad.Log hiding (Handler, Error)
import           Control.Monad.Trans.Control
import           Data.Aeson hiding (Error)
import           Data.Monoid ((<>))
import           Data.Proxy (Proxy)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Typeable
import           Llreve.Rise4Fun
import           Llreve.Run
import           Llreve.Solver
import           Llreve.Type
import           Llreve.Util
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Exit
import           System.IO (hClose)
import           System.IO.Temp

maxQueuedReqs :: Int
maxQueuedReqs = 10

maxConcurrentReqs :: Int
maxConcurrentReqs = 4

data Method
  = Solver !SMTSolver
  | Dynamic
  | Race

-- | Strict pair
data StrictPair a b = Pair !a !b deriving (Show, Eq, Ord)

data Request = Request
  { reqMethod :: !Method
  , reqFiles :: !(StrictPair Text Text)
  , reqPatterns :: !Text
  }

instance FromJSON Method where
  parseJSON "z3" = pure (Solver Z3)
  parseJSON "eldarica" = pure (Solver Eldarica)
  parseJSON "dynamic" = pure Dynamic
  parseJSON "race" = pure Race
  parseJSON _ = empty

instance FromJSON Request where
  parseJSON (Object v) = do
    method <- v .: "method"
    prog1 <- v .: "program1"
    prog2 <- v .: "program2"
    patterns <- v .: "patterns"
    pure (Request method (Pair prog1 prog2) patterns)
  parseJSON _ = empty

type LlreveAPI = ReqBody '[JSON] Request :> Post '[JSON] Response

type API = "llreve" :> (LlreveAPI :<|> Rise4funAPI)

-- Lifted version of mask because I don’t want to depend on lifted-base just for this function
mask' :: MonadBaseControl IO m => ((m a -> m a) -> m b) -> m b
mask' f = control $ \runInBase ->
           mask $ \g -> runInBase $ f $ liftBaseOp_ g

loggingHandler :: MonadIO m => Log.Handler m LogMessage'
loggingHandler = liftIO . print

llreveServer :: Maybe String -> Sem  -> Sem -> Server LlreveAPI
llreveServer includeDir queuedReqs concurrentReqs (Request method (Pair prog1 prog2) patterns) =
  withQueuedSem queuedReqs $
  withSystemTempFile "prog1.c" $ \file1 prog1Handle ->
    withSystemTempFile "prog2.c" $ \file2 prog2Handle ->
      withSystemTempFile "query.smt2" $ \smtFile smtHandle -> do
        liftIO $ do
          Text.hPutStr prog1Handle prog1
          Text.hPutStr prog2Handle prog2
                -- We can close these handles early
          hClose prog1Handle
          hClose prog2Handle
          hClose smtHandle
        withSem concurrentReqs $
          case method of
            Solver solver' -> do
              let responseMethod = SolverResponse solver'
              (llreveOutput, llreveSuccessful) <-
                runLlreve
                  file1
                  file2
                  smtFile
                  (llreveArgsForSolver solver')
                  includeDir
              if not llreveSuccessful
                then pure (llreveError llreveOutput responseMethod)
                else do
                  solverOutput <-
                    runSolver file1 file2 smtFile (solverConfig solver')
                  pure (Response llreveOutput solverOutput responseMethod)
            Dynamic -> runLlreveDynamic file1 file2 patterns smtFile includeDir
            Race -> raceResponse <$> raceSolvers file1 file2 patterns includeDir

raceResponse :: RaceResult -> Response
raceResponse (SolverResult _ resp) = resp
raceResponse (LlreveDynamicResult resp) = resp

data RaceResult
  = SolverResult !SMTSolver
                 !Response
  | LlreveDynamicResult !Response

raceSolvers
  :: (MonadIO m, MonadLog LogMessage' m, MonadMask m, MonadBaseControl IO m)
  => FilePath -> FilePath -> Text -> Maybe String -> m RaceResult
raceSolvers prog1 prog2 patterns includeDir = do
  either id LlreveDynamicResult <$>
    race
      raceSMTSolvers
      (withSystemTempFile "query.smt2" $ \smtFile smtHandle -> do
         liftIO $ hClose smtHandle
         runLlreveDynamic prog1 prog2 patterns smtFile includeDir)
  where
    raceSMTSolvers =
      either (SolverResult Eldarica) (SolverResult Z3) <$>
      race (runSolver' Eldarica) (runSolver' Z3)
    runSolver' solver' = do
      withSystemTempFile "query.smt2" $ \smtFile smtHandle -> do
        liftIO $ hClose smtHandle
        (llreveOutput, llreveSuccessful) <-
          runLlreve prog1 prog2 smtFile (llreveArgsForSolver solver') includeDir
        if not llreveSuccessful
          then pure (llreveError llreveOutput method)
          else do
            solverOutput <- runSolver prog1 prog2 smtFile (solverConfig solver')
            pure (Response llreveOutput solverOutput method)
      where
        method = SolverResponse solver'


llreveDynamicBinary :: String
llreveDynamicBinary = "llreve-dynamic"

parseLlreveDynamicResult :: Text -> LlreveResult
parseLlreveDynamicResult output =
  if "The programs have been proven equivalent" `elem` Text.lines output
    then Equivalent
    else Unknown

runLlreveDynamic
  :: (MonadIO m, MonadMask m)
  => FilePath -> FilePath -> Text -> FilePath -> Maybe String -> m Response
runLlreveDynamic prog1 prog2 patterns smtPath includeDir = do
  withSystemTempFile "patterns" $ \patternFile patternHandle -> do
    liftIO $ do
      Text.hPutStr patternHandle patterns
      hClose patternHandle
    processResult <-
      liftIO $
      readProcessWithTimeout
        (maxTimeout * (10 ^ (6 :: Int)))
        llreveDynamicBinary
        ([prog1, prog2, "-patterns", patternFile, "-o", smtPath] ++ includeArgs)
        ""
    -- for now we do not return LLVM ir this case
    let llreveOutput outp = LlreveOutput outp ("", "") ""
    case processResult of
      (output, Nothing) -> do
        pure
          (Response
             (llreveOutput output)
             (SolverOutput "" [] Timeout)
             DynamicResponse)
      (output, Just exit) ->
        case exit of
          ExitSuccess -> do
            smtFile <- liftIO $ Text.readFile smtPath
            pure
              (Response
                 (LlreveOutput output ("", "") smtFile)
                 (SolverOutput "" [] (parseLlreveDynamicResult output))
                 DynamicResponse)
          ExitFailure _
             -- TODO: logging support
           -> pure (llreveError (llreveOutput output) DynamicResponse)
  where
    includeArgs :: [String]
    includeArgs =
      case includeDir of
        Nothing -> []
        Just dir -> ["-I", dir]

llreveAPI :: Proxy API
llreveAPI = Proxy

verifyExecutable :: FilePath -> IO Bool
verifyExecutable path =
  catch
    (readProcessWithExitCode path [] "" >> pure True)
    (\(_ :: IOException) ->
       putStrLn
         ("Error: Couldn’t execute " <> path <>
          ". Please check that the executable is in your path and marked as executable") >>
       pure False)

verifyExecutables :: IO ()
verifyExecutables = do
  llreveOk <- verifyExecutable "llreve"
  llreveDynamicOk <- verifyExecutable "llreve-dynamic"
  z3Ok <- verifyExecutable "z3"
  eldOk <- verifyExecutable "eld"
  when (not (and [llreveOk, llreveDynamicOk, z3Ok, eldOk])) exitFailure

main :: IO ()
main = do
  verifyExecutables
  queuedReqsSem <- newSem maxQueuedReqs
  concurrentReqsSem <- newSem maxConcurrentReqs
  includeDir <- getStddefIncludeDir
  run 8080 $
    logStdoutDev $
    cors
      (const $
       Just $ simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}) $
    serve
      llreveAPI
      (llreveServer includeDir queuedReqsSem concurrentReqsSem :<|>
       rise4funServer includeDir queuedReqsSem concurrentReqsSem)
