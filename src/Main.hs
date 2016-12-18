{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Applicative
import           Control.Concurrent.Sem
import           Control.Monad
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
import           Data.Text.Encoding
import qualified Data.Text.IO as Text
import           Debug.Trace
import           Horname
import           Llreve.Util
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           System.Exit
import           System.IO (hClose)
import           System.IO.Temp
import           System.Process hiding (readProcessWithExitCode, readCreateProcessWithExitCode)
import           Text.Regex.Applicative.Text

maxQueuedReqs :: Int
maxQueuedReqs = 5

maxTimeout :: Int
maxTimeout = 15

maxConcurrentReqs :: Int
maxConcurrentReqs = 2

data SMTSolver = Eldarica | Z3
data Method = Solver !SMTSolver | Dynamic

-- | Strict pair
data StrictPair a b = Pair !a !b deriving (Show, Eq, Ord)

data Request = Request
  { method :: !Method
  , files :: !(StrictPair Text Text)
  , patterns :: !Text
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
  , llreveOutput :: !Text
  , solverOutput :: !Text
  , smt :: !Text
  , respInvariants :: ![DefineFun]
  }

instance ToJSON Response where
  toJSON (Response result llreve solver smt invariants) =
    object
      [ "result" .= result
      , "invariants" .= map ppDefineFun invariants
      , "llreve-output" .= llreve
      , "solver-output" .= solver
      , "smt" .= smt
      ]

type LlreveAPI = "llreve" :> ReqBody '[JSON] Request :> Post '[JSON] Response

type LogMessage' = WithSeverity LogMessage

data LlreveInput = LlreveInput
  { inpProgram1 :: !Text
  , inpProgram2 :: !Text
  } deriving (Show, Eq, Ord)

data ProgramOutput = ProgramOutput
  { progOut :: !Text
  } deriving (Show, Eq, Ord)

newtype SMTFile =
  SMTFile Text
  deriving (Show, Eq, Ord)

data LogMessage
  = LlreveMsg { llreveMsg :: !Text
             ,  llreveOutp :: !ProgramOutput
             ,  llreveInp :: !LlreveInput}
  | Z3Msg { z3Msg :: !Text
         ,  z3Inp :: !SMTFile
         ,  z3Outp :: !ProgramOutput
         ,  llreveInp :: !LlreveInput}
  | HornameMsg { hornameMsg :: !Text
              ,  hornameSolverInp :: !Text
              ,  hornameSolverOutp :: !Text}
  | EldaricaMsg { eldaricaMsg :: !Text
               ,  eldInput :: !SMTFile
               ,  eldOutp :: !ProgramOutput
               ,  llreveInp :: !LlreveInput}
  deriving (Show, Eq, Ord)

llreveArgsForSolver :: SMTSolver -> [String]
llreveArgsForSolver Z3 = ["-muz"]
llreveArgsForSolver Eldarica = []

server :: Server LlreveAPI
server (Request method (Pair prog1 prog2) patterns) =
  enter
    (Nat (\app -> runLoggingT app (liftIO . print)) :: LoggingT LogMessage' Handler :~> Handler) $
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
              Solver solver -> do
                resp <-
                  runLlreve file1 file2 smtFile (llreveArgsForSolver solver)
                case resp of
                  Left resp -> pure resp
                  Right llreveOutp ->
                    runSolver
                      file1
                      file2
                      smtFile
                      llreveOutp
                      (solverConfig solver)
              Dynamic -> runLlreveDynamic file1 file2 patterns smtFile

llreveBinary :: String
llreveBinary = "llreve"

-- In the case of an error Left is returned
runLlreve
  :: (MonadIO m, MonadError ServantErr m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> [String] -> m (Either Response Text)
runLlreve prog1 prog2 smt llreveArgs = do
  (exit, llreveOut) <-
    liftIO $
    readProcessWithExitCode
      llreveBinary
      (prog1 : prog2 : "-o" : smt : "-inline-opts" : llreveArgs)
      ""
  case exit of
    ExitSuccess -> pure (Right llreveOut)
    ExitFailure _ -> do
      llreveInp <- llreveInput prog1 prog2
      logError (LlreveMsg "llreve failed" (ProgramOutput llreveOut) llreveInp)
      pure (Left (Response Error llreveOut "" "" []))

llreveInput :: MonadIO m => FilePath -> FilePath -> m LlreveInput
llreveInput prog1 prog2 = do
  liftIO $ LlreveInput <$> Text.readFile prog1 <*> Text.readFile prog2

findInvariants
  :: (MonadIO m, MonadLog LogMessage' m)
  => LlreveResult -> FilePath -> Text -> m [DefineFun]
findInvariants result smtPath solverOutp =
  case result of
    Equivalent -> do
      smtInput <- liftIO $ Text.readFile smtPath
      case extractRenamedInvariants smtPath smtInput "z3_output" solverOutp of
        Right invariants -> pure invariants
        Left _ -> do
          logError
            (HornameMsg
               "Couldn’t parse and rename invariants"
               smtInput
               solverOutp)
          pure []
    _ -> pure []


z3Binary :: String
z3Binary = "z3"

parseZ3Result :: Text -> LlreveResult
parseZ3Result output =
  case findFirstInfix
         (("unsat" *> pure Equivalent) <|> ("sat" *> pure NotEquivalent))
         output of
    Just (_, result, _) -> result
    Nothing -> Error

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

data SolverConfig = SolverCfg
  { solverBinaryPath :: String
  , solverParseResult :: Text -> LlreveResult
  , solverArgs :: FilePath -> [String]
  , solverLogMsg :: Text -> SMTFile -> ProgramOutput -> LlreveInput -> LogMessage
  }

solverConfig :: SMTSolver -> SolverConfig
solverConfig Z3 =
  SolverCfg z3Binary parseZ3Result (: ["fixedpoint.engine=duality"]) Z3Msg
solverConfig Eldarica =
  SolverCfg eldaricaBinary parseEldaricaResult (: ["-ssol"]) EldaricaMsg

runSolver
  :: (MonadIO m, MonadError ServantErr m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> Text -> SolverConfig -> m Response
runSolver prog1 prog2 smtPath llreveOutp solver = do
  (exit, solverOutp) <-
    liftIO $
    readProcessWithExitCode
      (solverBinaryPath solver)
      (solverArgs solver smtPath)
      ""
  case exit of
    ExitSuccess -> do
      let result = solverParseResult solver solverOutp
      invariants <- findInvariants result smtPath solverOutp
      smt <- liftIO $ Text.readFile smtPath
      pure (Response result llreveOutp solverOutp smt invariants)
    ExitFailure _ -> do
      llreveInp <- llreveInput prog1 prog2
      smtInp <- liftIO $ Text.readFile smtPath
      logError
        (solverLogMsg
           solver
           "Non-zero exit code"
           (SMTFile smtInp)
           (ProgramOutput solverOutp)
           llreveInp)
      throwError err500

llreveDynamicBinary :: String
llreveDynamicBinary = "llreve-dynamic"

parseLlreveDynamicResult :: Text -> LlreveResult
parseLlreveDynamicResult output =
  if "The programs have been proven equivalent" `elem` Text.lines output
    then Equivalent
    else Unknown

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
    (exit, outp) <-
      liftIO $
      readProcessWithExitCode
        llreveDynamicBinary
        [prog1, prog2, "-patterns", patternFile, "-o", smtPath]
        ""
    case exit of
      ExitSuccess -> do
        smt <- liftIO $ Text.readFile smtPath
        pure (Response (parseLlreveDynamicResult outp) outp "" smt [])
      ExitFailure _
        -- TODO: logging support
       -> throwError err500

llreveAPI :: Proxy LlreveAPI
llreveAPI = Proxy

main = do
  run 8080 $
    logStdoutDev $
    cors
      (const $
       Just $ simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}) $
    serve llreveAPI server
