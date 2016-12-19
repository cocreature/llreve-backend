module Llreve.Util
  ( liftBaseOp2
  , readProcessWithExitCode
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Control
import qualified Data.ByteString as ByteString
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           System.Exit
import           System.IO
import           System.Process hiding (readProcessWithExitCode, readCreateProcessWithExitCode)

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
