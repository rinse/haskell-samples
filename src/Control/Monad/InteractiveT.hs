{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.InteractiveT where

import           Control.Arrow        ((|||))
import           Control.Monad.Except


-- |Interact Monad.
newtype InteractT m a = InteractT
    { unInteractT :: ExceptT String m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runInteractT :: InteractT m a -> m (Either String a)
runInteractT = runExceptT . unInteractT

-- |Escapes from 'interactive'.
quit :: Monad m => String -> InteractT m String
quit = InteractT . ExceptT . return . Left

interactive :: Monad m => m String -> (String -> m ()) -> (String -> m String) -> m ()
interactive getAction putAction f = getAction >>= f >>= putAction >> interactive getAction putAction f

-- |Interactive action.
interactive' :: Monad m
             => m String                        -- ^ get a monadic string as an input.
             -> (String -> m ())                -- ^ put a string in a monadic action.
             -> (String -> InteractT m String)  -- ^ A monadic action takes an input and returns an output.
             -> m ()
interactive' getAction putAction f =
    runInteractT (interactive (lift getAction) (lift . putAction) f)
    >>= putAction ||| return
