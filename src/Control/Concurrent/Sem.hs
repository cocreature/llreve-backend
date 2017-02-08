module Control.Concurrent.Sem
  ( Sem
  , newSem
  , waitSem
  , tryWaitSem
  , signalSem
  , withSem
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

newtype Sem =
  Sem (TVar Int)
  deriving Eq

newSem :: Int -> IO Sem
newSem i = fmap Sem (newTVarIO i)

waitSem :: Sem -> IO ()
waitSem (Sem t) =
  atomically $ do
    i <- readTVar t
    when (i <= 0) retry
    writeTVar t $! (i - 1)

tryWaitSem :: Sem -> IO Bool
tryWaitSem (Sem t) =
  atomically $ do
    i <- readTVar t
    if (i <= 0)
      then pure False
      else (writeTVar t $! (i - 1)) >> pure True

signalSem :: Sem -> IO ()
signalSem (Sem t) =
  atomically $ do
    i <- readTVar t
    writeTVar t $! i + 1

withSem :: (MonadMask m, MonadIO m) => Sem -> m c -> m c
withSem sem = bracket_ (liftIO (waitSem sem)) (liftIO (signalSem sem))
