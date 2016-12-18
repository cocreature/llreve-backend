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

data LogMessage
  = LlreveMsg { llreveMsg :: !Text }
  | Z3Msg { z3Msg :: !Text }
  | HornameMsg { hornameMsg :: !Text }
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
              Eldarica -> pure (Response Error [])
              Dynamic -> pure (Response Error [])

llreveBinary :: String
llreveBinary = "reve"

runLlreve
  :: (MonadIO m, MonadError ServantErr m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> [String] -> m ()
runLlreve prog1 prog2 smt llreveArgs = do
  (exit, out, err) <-
    liftIO $ readProcessWithExitCode "reve" (prog1 : prog2 : "-o" : smt : llreveArgs) ""
  case exit of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      logError (LlreveMsg "llreve failed")
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

runZ3
  :: (MonadIO m, MonadError ServantErr m, MonadLog LogMessage' m)
  => FilePath -> FilePath -> FilePath -> m Response
runZ3 prog1 prog2 smt = do
  runLlreve prog1 prog2 smt ["-muz"]
  (exit, out, err) <-
    liftIO $ readProcessWithExitCode "z3" ["fixedpoint.engine=duality", smt] ""
  case exit of
    ExitSuccess -> do
      case parseZ3Result out of
        Equivalent -> do
          smtInput <- liftIO $ Text.readFile smt
          case extractRenamedInvariants smt smtInput "z3_output" out of
            Right invariants -> pure (Response Equivalent invariants)
            Left _ -> do
              logError (HornameMsg "Couldn’t parse and rename invariants")
              pure (Response Equivalent [])
        NotEquivalent -> pure (Response NotEquivalent [])
        Unknown -> pure (Response Unknown [])
        Error -> pure (Response Error [])
    ExitFailure _ -> do
      logError (Z3Msg "Z3 failed")
      throwError err500

llreveAPI :: Proxy LlreveAPI
llreveAPI = Proxy

main = do
  run 8080 $ logStdoutDev $ serve llreveAPI server
