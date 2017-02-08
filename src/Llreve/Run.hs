{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Llreve.Run
  ( runLlreve
  , llreveArgsForSolver
  , withQueuedSem
  ) where

import Control.Concurrent.Sem
import Control.Monad.Catch hiding (Handler)
import Control.Monad.IO.Class
import Control.Monad.Log hiding (Handler, Error)
import Data.Text (Text)
import Llreve.Solver
import Llreve.Type
import Llreve.Util
import Servant
import System.Exit

llreveBinary :: String
llreveBinary = "llreve"

llreveArgsForSolver :: SMTSolver -> [String]
llreveArgsForSolver Z3 = ["-muz"]
llreveArgsForSolver Eldarica = []

-- In the case of an error Left is returned
runLlreve
  :: (MonadIO m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> [String] -> Maybe String -> ResponseMethod -> m (Either Response Text)
runLlreve prog1 prog2 smtPath llreveArgs includeDir method = do
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
      pure (Left (Response Error llreveOut "" "" [] method))
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
