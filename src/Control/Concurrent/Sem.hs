module Control.Concurrent.Sem
  ( Sem
  , newSem
  , waitSem
  , tryWaitSem
  , signalSem
  ) where

import Control.Monad
import Control.Concurrent.STM

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
