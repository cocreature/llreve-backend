{-# LANGUAGE OverloadedStrings #-}
module Llreve.Util
  ( liftBaseOp2
  , readProcessWithExitCode
  , readProcessWithTimeout
  , getStddefIncludeDir
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.Control
import qualified Data.ByteString as ByteString
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           System.Exit
import           System.IO
import           System.Process hiding (readProcessWithExitCode, readCreateProcessWithExitCode)
import           System.Timeout
import           Text.Regex.Applicative.Text

liftBaseOp2 :: MonadBaseControl b m
            => ((a -> a' -> b (StM m c)) -> b (StM m d))
            -> ((a -> a' ->        m c)  ->        m d)
liftBaseOp2 f = \g -> control $ \runInBase -> f $ \a a' -> runInBase (g a a')

readProcessWithExitCode :: String -> [String] -> Text -> IO (ExitCode, Text)
readProcessWithExitCode cmd args =
  readCreateProcessWithExitCode (proc cmd args)

readCreateProcessWithExitCode
  :: CreateProcess
  -> Text -- ^ standard input
  -> IO (ExitCode, Text) -- ^ exitcode, interleaved stdout, stderr
readCreateProcessWithExitCode cp input =
  bracket createPipe (\(readEnd, writeEnd) -> hClose readEnd >> hClose writeEnd) $ \(readEnd, writeEnd) -> do
    let cp_opts =
          cp
          { std_in = CreatePipe
          , std_out = UseHandle writeEnd
          , std_err = UseHandle writeEnd
          }
    withCreateProcess cp_opts $ \(Just inh) Nothing Nothing ph -> do
      unless (Text.null input) $ Text.hPutStr inh input
      hClose inh
      out <- Text.decodeUtf8 <$> ByteString.hGetContents readEnd
      hClose readEnd
      ex <- waitForProcess ph
      return (ex, out)

readProcessWithTimeout ::
  Int -> String -> [String] -> Text -> IO (Text, (Maybe ExitCode))
readProcessWithTimeout delay cmd args =
  readCreateProcessWithTimeout delay (proc cmd args)

readCreateProcessWithTimeout ::
  Int -> CreateProcess -> Text -> IO (Text, (Maybe ExitCode))
readCreateProcessWithTimeout delay cp input = do
  processOutput <- newTChanIO
  processResult <-
    timeout delay (readCreateProcessWithExitCode' cp input processOutput)
  output <- Text.unlines <$> atomically (getChanContents processOutput)
  pure (output, processResult)
  where
    getChanContents :: TChan a -> STM [a]
    getChanContents chan = do
      empty' <- isEmptyTChan chan
      if empty'
        then pure []
        else do
          x <- readTChan chan
          xs <- getChanContents chan
          pure (x : xs)

readCreateProcessWithExitCode'
  :: CreateProcess
  -> Text -- ^ standard input
  -> TChan Text -- ^ interleaved stdout, stderr separated by lines
  -> IO ExitCode -- ^ exitcode
readCreateProcessWithExitCode' cp input output = do
  bracket createPipe (\(readEnd, writeEnd) -> hClose readEnd >> hClose writeEnd) $ \(readEnd, writeEnd) -> do
    let cp_opts =
          cp
          { std_in = CreatePipe
          , std_out = UseHandle writeEnd
          , std_err = UseHandle writeEnd
          }
    withCreateProcess cp_opts $ \(Just inh) Nothing Nothing ph -> do
      unless (Text.null input) $ Text.hPutStr inh input
      hClose inh
      withAsync
        (forever
           (atomically . writeTChan output . Text.decodeUtf8 =<<
            ByteString.hGetLine readEnd)) $ \_ -> do
        ex <- waitForProcess ph
        return ex

getStddefIncludeDir :: IO (Maybe String)
getStddefIncludeDir = do
  (_, outp) <- readProcessWithExitCode "gcc" ["-E", "-"] "#include<stddef.h>\n"
  case findFirstInfix
         (sym '"' *> many (psym (/= '"')) <* "stddef.h" <* sym '"')
         outp of
    Nothing -> pure Nothing
    Just (_, includeDir, _) -> pure (Just includeDir)
