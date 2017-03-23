{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Llreve.Solver where

import           Control.Monad.Except
import           Control.Monad.Log hiding (Error)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Horname
import           Llreve.Type
import           Llreve.Util
import           System.Exit
import           Text.Regex.Applicative.Text

maxTimeout :: Int
maxTimeout = 15

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
  { solver :: SMTSolver
  , solverBinaryPath :: String
  , solverParseResult :: Text -> LlreveResult
  , solverArgs :: FilePath -> [String]
  , solverLogMsg :: Text -> SMTFile -> ProgramOutput -> LlreveInput -> LogMessage
  }

solverConfig :: SMTSolver -> SolverConfig
solverConfig Z3 =
  SolverCfg
    Z3
    z3Binary
    parseZ3Result
    (: ["fixedpoint.engine=duality", "-T:" <> show maxTimeout])
    Z3Msg
solverConfig Eldarica =
  SolverCfg
    Eldarica
    eldaricaBinary
    parseEldaricaResult
    (: ["-ssol", "-t:" <> show maxTimeout])
    EldaricaMsg

runSolver
  :: (MonadIO m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> LlreveOutput -> SolverConfig -> m Response
runSolver prog1 prog2 smtPath llreveOut solverConf = do
  (exit, solverOutp) <-
    liftIO $
    readProcessWithExitCode
      (solverBinaryPath solverConf)
      (solverArgs solverConf smtPath)
      ""
  case exit of
    ExitSuccess -> do
      let result = solverParseResult solverConf solverOutp
      invariants <- findInvariants result smtPath solverOutp
      smt' <- liftIO $ Text.readFile smtPath
      pure (Response result llreveOut solverOutp smt' invariants (SolverResponse (solver solverConf)))
    ExitFailure _ -> do
      llreveIn <- llreveInput prog1 prog2
      smtInp <- liftIO $ Text.readFile smtPath
      logError
        (solverLogMsg
           solverConf
           "Non-zero exit code"
           (SMTFile smtInp)
           (ProgramOutput solverOutp)
           llreveIn)
      pure (Response Error llreveOut solverOutp smtInp [] (SolverResponse (solver solverConf)))

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

llreveInput :: MonadIO m => FilePath -> FilePath -> m LlreveInput
llreveInput prog1 prog2 = do
  liftIO $ LlreveInput <$> Text.readFile prog1 <*> Text.readFile prog2
