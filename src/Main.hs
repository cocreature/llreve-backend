{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Applicative
import           Control.Concurrent.Sem
import           Control.Monad.Catch hiding (Handler)
import           Control.Monad.Except
import qualified Control.Monad.Log as Log
import           Control.Monad.Log hiding (Handler, Error)
import           Control.Monad.Trans.Control
import           Data.Aeson hiding (Error)
import           Data.Proxy (Proxy)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
import           System.Timeout

maxQueuedReqs :: Int
maxQueuedReqs = 5

maxConcurrentReqs :: Int
maxConcurrentReqs = 2

data Method = Solver !SMTSolver | Dynamic

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
  parseJSON _ = empty

instance FromJSON Request where
  parseJSON (Object v) = do
    method <- v .: "method"
    prog1 <- v .: "program1"
    prog2 <- v .: "program2"
    patterns <- v .: "patterns"
    pure (Request method (Pair prog1 prog2) patterns)
  parseJSON _ = empty

type LlreveAPI = "llreve" :> ReqBody '[JSON] Request :> Post '[JSON] Response

llreveArgsForSolver :: SMTSolver -> [String]
llreveArgsForSolver Z3 = ["-muz"]
llreveArgsForSolver Eldarica = []

-- Lifted version of mask because I don’t want to depend on lifted-base just for this function
mask' :: MonadBaseControl IO m => ((m a -> m a) -> m b) -> m b
mask' f = control $ \runInBase ->
           mask $ \g -> runInBase $ f $ liftBaseOp_ g

loggingHandler :: MonadIO m => Log.Handler m LogMessage'
loggingHandler = liftIO . print

withQueuedSem :: Sem -> LoggingT LogMessage' IO c -> Handler c
withQueuedSem queuedReqs action = do
  res <- liftIO $ mask $ \unmasked -> do
    lock <- tryWaitSem queuedReqs
    if lock
      then do
        result <- unmasked (runLoggingT action (liftIO . print)) `onException` (signalSem queuedReqs)
        signalSem queuedReqs
        return (Right result)
      else pure (Left err503)
  case res of
    Right res' -> pure res'
    Left err -> throwError err

server :: Maybe String -> Sem  -> Sem -> Server LlreveAPI
server includeDir queuedReqs concurrentReqs (Request method (Pair prog1 prog2) patterns) =
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
        bracket_
          (liftIO $ waitSem concurrentReqs)
          (liftIO $ signalSem concurrentReqs) $
          case method of
            Solver solver -> do
              resp <-
                runLlreve
                  file1
                  file2
                  smtFile
                  (llreveArgsForSolver solver)
                  includeDir
              case resp of
                Left resp' -> pure resp'
                Right llreveOut ->
                  runSolver file1 file2 smtFile llreveOut (solverConfig solver)
            Dynamic -> runLlreveDynamic file1 file2 patterns smtFile includeDir

llreveBinary :: String
llreveBinary = "llreve"

-- In the case of an error Left is returned
runLlreve
  :: (MonadIO m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> [String] -> Maybe String -> m (Either Response Text)
runLlreve prog1 prog2 smtPath llreveArgs includeDir = do
  (exit, llreveOut) <-
    liftIO $
    readProcessWithExitCode
      llreveBinary
      (prog1 :
       prog2 : "-o" : smtPath : "-inline-opts" : includeArgs ++ llreveArgs)
      ""
  case exit of
    ExitSuccess -> pure (Right llreveOut)
    ExitFailure _ -> do
      llreveIn <- llreveInput prog1 prog2
      logError (LlreveMsg "llreve failed" (ProgramOutput llreveOut) llreveIn)
      pure (Left (Response Error llreveOut "" "" []))
  where
    includeArgs :: [String]
    includeArgs =
      case includeDir of
        Nothing -> []
        Just dir -> ["-I", dir]

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
      readProcessWithTimeout (maxTimeout * (10 ^ (6 :: Int)))
        llreveDynamicBinary
        ([prog1, prog2, "-patterns", patternFile, "-o", smtPath] ++ includeArgs)
        ""
    case processResult of
      (output, Nothing) -> do
        pure (Response Timeout output "" "" [])
      (output, Just exit) ->
        case exit of
          ExitSuccess -> do
            smtFile <- liftIO $ Text.readFile smtPath
            pure (Response (parseLlreveDynamicResult output) output "" smtFile [])
          ExitFailure _
             -- TODO: logging support
           -> pure (Response Error output "" "" [])
  where
    includeArgs :: [String]
    includeArgs =
      case includeDir of
        Nothing -> []
        Just dir -> ["-I", dir]

llreveAPI :: Proxy LlreveAPI
llreveAPI = Proxy

main :: IO ()
main = do
  queuedReqsSem <- newSem maxQueuedReqs
  concurrentReqsSem <- newSem maxConcurrentReqs
  includeDir <- getStddefIncludeDir
  run 8080 $
    logStdoutDev $
    cors
      (const $
       Just $ simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}) $
    serve llreveAPI (server includeDir queuedReqsSem concurrentReqsSem)
