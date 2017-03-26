{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Llreve.Run
  ( runLlreve
  , llreveArgsForSolver
  , withQueuedSem
  ) where

import           Control.Concurrent.Sem
import           Control.Monad.Catch hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Log hiding (Handler, Error)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Llreve.Solver
import           Llreve.Type
import           Llreve.Util
import           Servant
import           System.Exit
import           System.IO
import           System.IO.Temp

llreveBinary :: String
llreveBinary = "llreve"

llreveArgsForSolver :: SMTSolver -> [String]
llreveArgsForSolver Z3 = ["-muz"]
llreveArgsForSolver Eldarica = []

readLLVMIR :: (String, String) -> IO (Text, Text)
readLLVMIR (ir1, ir2) = do
  (,) <$> Text.readFile ir1 <*> Text.readFile ir2

-- | The 'Bool' indicates whether llreve ran successfully
runLlreve
  :: (MonadIO m, MonadLog LogMessage' m)
  => FilePath
  -> FilePath
  -> FilePath
  -> [String]
  -> Maybe String
  -> m (LlreveOutput, Bool)
runLlreve prog1 prog2 smtPath llreveArgs includeDir = do
  (exit, llreveOut) <-
    liftIO $
    withSystemTempFile "ir1.ll" $ \ir1File ir1Handle -> do
      hClose ir1Handle
      withSystemTempFile "ir2.ll" $ \ir2File ir2Handle -> do
        hClose ir2Handle
        (exit, llreveOut) <-
          liftIO $
          readProcessWithExitCode
            llreveBinary
            ([ prog1
             , prog2
             , "-o"
             , smtPath
             , "-write-ir-1"
             , ir1File
             , "-write-ir-2"
             , ir2File
             , "-inline-opts"
             ] ++
             includeArgs ++ llreveArgs)
            ""
        llvmIR <- readLLVMIR (ir1File, ir2File)
        generatedSMT <- Text.readFile smtPath
        pure (exit, LlreveOutput llreveOut llvmIR generatedSMT)
  case exit of
    ExitSuccess -> pure (llreveOut, True)
    ExitFailure _ -> do
      llreveIn <- llreveInput prog1 prog2
      logError
        (LlreveMsg
           "llreve failed"
           (ProgramOutput (llreveStdout llreveOut))
           llreveIn)
      pure (llreveOut, False)
  where
    includeArgs :: [String]
    includeArgs =
      case includeDir of
        Nothing -> []
        Just dir -> ["-I", dir]

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
