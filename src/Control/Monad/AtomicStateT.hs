{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Control.Monad.AtomicStateT where

import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Control.Monad.Reader
import           Control.Monad.State.Class


newtype AtomicStateT s m a = AtomicStateT
    { unAtomicStateT :: ReaderT (TVar s) m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow)

runAtomicStateT :: MonadIO m => AtomicStateT s m a -> TVar s -> m (a, s)
runAtomicStateT st s = (,) <$> runReaderT (unAtomicStateT st) s <*> liftIO (readTVarIO s)

modifyAtomically :: MonadIO m => (s -> s) -> AtomicStateT s m ()
modifyAtomically f = AtomicStateT $ do
    p <- ask
    liftIO . atomically $ readTVar p >>= writeTVar p . f

instance MonadIO m => MonadState s (AtomicStateT s m) where
    get = AtomicStateT $ ask >>= liftIO . readTVarIO
    put s = AtomicStateT $ ask >>= liftIO . atomically . flip writeTVar s
