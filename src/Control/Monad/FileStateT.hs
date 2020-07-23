{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Control.Monad.FileStateT where

import           Control.DeepSeq
import           Control.Exception.Safe
import           Control.Monad.Reader
import           Control.Monad.State.Class


newtype FileStateT s m a = FileStateT
    { unFileStateT :: ReaderT String m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow)

runFileStateT :: (MonadIO m, Read s, NFData s) => FileStateT s m a -> FilePath -> m (a, s)
runFileStateT (FileStateT r) filePath = do
    a <- runReaderT r filePath
    s <- read <$> liftIO (readFile filePath)
    s `deepseq` return (a, s)

instance (MonadIO m, Read s, Show s, NFData s) => MonadState s (FileStateT s m) where
    get = FileStateT $ do
        s <- ask >>= liftIO . readFile
        s `deepseq` return $ read s
    put s = FileStateT $ do
        filePath <- ask
        liftIO . writeFile filePath $ show s
