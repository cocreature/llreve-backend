{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Applicative
import           Control.Concurrent.Sem
import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Control.Monad.Log as Log
import           Control.Monad.Log hiding (Handler, Error)
import           Control.Monad.Trans.Control
import           Data.Aeson hiding (Error)
import           Data.Monoid
import           Data.Proxy (Proxy)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Debug.Trace
import           Horname
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Exit
import           System.IO (hClose)
import           System.IO.Temp
import           System.Process.Text
import           Text.Regex.Applicative.Text

maxQueuedReqs :: Int
maxQueuedReqs = 5

maxTimeout :: Int
maxTimeout = 15

maxConcurrentReqs :: Int
maxConcurrentReqs = 2

liftBaseOp2 :: MonadBaseControl b m
            => ((a -> a' -> b (StM m c)) -> b (StM m d))
            -> ((a -> a' ->        m c)  ->        m d)
liftBaseOp2 f = \g -> control $ \runInBase -> f $ \a a' -> runInBase (g a a')

data Method = Z3 | Eldarica | Dynamic

-- | Strict pair
data StrictPair a b = Pair !a !b deriving (Show, Eq, Ord)

data Request = Request
  { method :: !Method
  , files :: !(StrictPair Text Text)
  , patterns :: !Text
  }

instance FromJSON Method where
  parseJSON "z3" = pure Z3
  parseJSON "eldarica" = pure Eldarica
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

data LlreveResult
  = Equivalent
  | NotEquivalent
  | Unknown
  | Error

instance ToJSON LlreveResult where
  toJSON Equivalent = "equivalent"
  toJSON NotEquivalent = "not-equivalent"
  toJSON Unknown = "unknown"
  toJSON Error = "error"

data Response = Response
  { respResult :: !LlreveResult
  , respInvariants :: ![DefineFun]
  }

instance ToJSON Response where
  toJSON (Response result invariants) =
    object ["result" .= result, "invariants" .= map ppDefineFun invariants]

type LlreveAPI = "llreve" :> ReqBody '[JSON] Request :> Post '[JSON] Response

type LogMessage' = WithSeverity LogMessage

data LlreveInput = LlreveInput
  { inpProgram1 :: !Text
  , inpProgram2 :: !Text
  } deriving (Show, Eq, Ord)

data ProgramOutput = ProgramOutput
  { progStdout :: !Text
  , progStderr :: !Text
  } deriving (Show, Eq, Ord)

data LogMessage
  = LlreveMsg { llreveMsg :: !Text
             ,  llreveOutp :: !ProgramOutput
             ,  llreveInp :: !LlreveInput}
  | Z3Msg { z3Msg :: !Text
         ,  z3Inp :: !Text
         ,  z3Outp :: !ProgramOutput
         ,  llreveInp :: !LlreveInput}
  | HornameMsg { hornameMsg :: !Text
              ,  hornameSolverInp :: !Text
              ,  hornameSolverOutp :: !Text}
  | EldaricaMsg { eldaricaMsg :: !Text
               ,  eldOutp :: !ProgramOutput
               ,  llreveInp :: !LlreveInput
               ,  eldInput :: !Text}
  deriving (Show, Eq, Ord)

server :: Server LlreveAPI
server (Request method (Pair prog1 prog2) patterns) =
  enter (Nat (\app -> runLoggingT app (liftIO . print))  :: LoggingT LogMessage' Handler :~> Handler) $
  server'
  where
    server' :: LoggingT LogMessage' Handler Response
    server' =
      liftBaseOp2 (withSystemTempFile "prog1.c") $ \file1 prog1Handle ->
        liftBaseOp2 (withSystemTempFile "prog2.c") $ \file2 prog2Handle ->
          liftBaseOp2 (withSystemTempFile "query.smt2") $ \smtFile smtHandle -> do
            liftIO $ do
              Text.hPutStr prog1Handle prog1
              Text.hPutStr prog2Handle prog2
                -- We can close these handles early
              hClose prog1Handle
              hClose prog2Handle
              hClose smtHandle
            case method of
              Z3 -> runZ3 file1 file2 smtFile
              Eldarica -> runEldarica file1 file2 smtFile
              Dynamic -> runLlreveDynamic file1 file2 patterns smtFile

llreveBinary :: String
llreveBinary = "llreve"

runLlreve
  :: (MonadIO m, MonadError ServantErr m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> [String] -> m ()
runLlreve prog1 prog2 smt llreveArgs = do
  (exit, llreveStdout, llreveStderr) <-
    liftIO $
    readProcessWithExitCode
      llreveBinary
      (prog1 : prog2 : "-o" : smt : "-inline-opts" : llreveArgs)
      ""
  case exit of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      llreveInp <- llreveInput prog1 prog2
      logError
        (LlreveMsg
           "llreve failed"
           (ProgramOutput llreveStdout llreveStderr)
           llreveInp)
      throwError err500

z3Binary :: String
z3Binary = "z3"

parseZ3Result :: Text -> LlreveResult
parseZ3Result output =
  case findFirstInfix
         (("unsat" *> pure Equivalent) <|> ("sat" *> pure NotEquivalent))
         output of
    Just (_, result, _) -> result
    Nothing -> Error

llreveInput :: MonadIO m => FilePath -> FilePath -> m LlreveInput
llreveInput prog1 prog2 = do
  liftIO $ LlreveInput <$> Text.readFile prog1 <*> Text.readFile prog2

addInvariants
  :: (MonadIO m, MonadLog LogMessage' m)
  => LlreveResult -> FilePath -> Text -> m Response
addInvariants result smtPath solverOutp =
  case result of
    Equivalent -> do
      smtInput <- liftIO $ Text.readFile smtPath
      case extractRenamedInvariants smtPath smtInput "z3_output" solverOutp of
        Right invariants -> pure (Response Equivalent invariants)
        Left _ -> do
          logError
            (HornameMsg
               "Couldn’t parse and rename invariants"
               smtInput
               solverOutp)
          pure (Response Equivalent [])
    NotEquivalent -> pure (Response NotEquivalent [])
    Unknown -> pure (Response Unknown [])
    Error -> pure (Response Error [])

runZ3
  :: (MonadIO m, MonadError ServantErr m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> m Response
runZ3 prog1 prog2 smtPath = do
  runLlreve prog1 prog2 smtPath ["-muz"]
  (exit, z3Stdout, z3Stderr) <-
    liftIO $
    readProcessWithExitCode z3Binary ["fixedpoint.engine=duality", smtPath] ""
  case exit of
    ExitSuccess -> addInvariants (parseZ3Result z3Stdout) smtPath z3Stdout
    ExitFailure _ -> do
      llreveInp <- llreveInput prog1 prog2
      smtInp <- liftIO $ Text.readFile smtPath
      logError
        (Z3Msg "Z3 failed" smtInp (ProgramOutput z3Stdout z3Stderr) llreveInp)
      throwError err500

eldaricaBinary :: String
eldaricaBinary = "eld"

parseEldaricaResult :: Text -> LlreveResult
parseEldaricaResult output =
  case findFirstInfix
         (("sat" *> pure Equivalent) <|> ("unsat" *> pure NotEquivalent) <|>
          ("unknown" *> pure Unknown))
         output of
    Just (_, result, _) -> result
    Nothing -> Error

runEldarica :: (MonadIO m, MonadError ServantErr m, MonadLog LogMessage' m)
            => FilePath -> FilePath -> FilePath -> m Response
runEldarica prog1 prog2 smtPath = do
  runLlreve prog1 prog2 smtPath []
  (exit, eldStdout, eldStderr) <-
    liftIO $ readProcessWithExitCode eldaricaBinary ["-ssol", smtPath] ""
  case exit of
    ExitSuccess ->
      addInvariants (parseEldaricaResult eldStdout) smtPath eldStdout
    ExitFailure _ -> do
      llreveInp <- llreveInput prog1 prog2
      smtInp <- liftIO $ Text.readFile smtPath
      logError
        (EldaricaMsg
           "Eldarica failed"
           (ProgramOutput eldStdout eldStderr)
           llreveInp
           smtInp)
      throwError err500

llreveDynamicBinary :: String
llreveDynamicBinary = "llreve-dynamic"

parseLlreveDynamicResult :: Text -> LlreveResult
parseLlreveDynamicResult output =
  if "The programs have been proven equivalent" `elem` (traceShowId $ Text.lines output) then
    Equivalent
  else
    Unknown

runLlreveDynamic
  :: FilePath
  -> FilePath
  -> Text
  -> FilePath
  -> LoggingT LogMessage' Handler Response
runLlreveDynamic prog1 prog2 patterns smtPath = do
  liftBaseOp2 (withSystemTempFile "patterns") $ \patternFile patternHandle -> do
    liftIO $ do
      Text.hPutStr patternHandle patterns
      hClose patternHandle
    (exit, stdout, stderr) <-
      liftIO $
      readProcessWithExitCode
        llreveDynamicBinary
        [prog1, prog2, "-patterns", patternFile, "-o", smtPath]
        ""
    case exit of
      ExitSuccess -> pure (Response (parseLlreveDynamicResult stderr) [])
      ExitFailure _
        -- TODO: logging support
       -> throwError err500

llreveAPI :: Proxy LlreveAPI
llreveAPI = Proxy

main = do
  run 8080 $ logStdoutDev $ serve llreveAPI server
