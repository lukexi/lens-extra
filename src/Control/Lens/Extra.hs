module Control.Lens.Extra 
  ( module Extra
  , module Control.Lens.Extra
  ) where

import Control.Lens as Extra hiding 
  ( (.=) 
  , (?=) 
  , (%=) 
  , (+=) 
  , (-=)
  , (*=) 
  ) 
import Control.Monad.State
import Control.Monad.Reader

forceState :: (MonadState s m) => m ()
forceState = do x <- get; x `seq` return ()

-- | (.=) using strict modify'
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= x = modify' (l .~ x)
{-# INLINE (.=) #-}
infix 4 .=

-- | (?=) using strict modify'
(?=) :: MonadState s m => ASetter s s a (Maybe b) -> b -> m ()
l ?= x = modify' (l ?~ x)
{-# INLINE (?=) #-}
infix 4 ?=

-- | (%=) using strict modify'
(%=) :: (Profunctor p, MonadState s m) => Setting p s s a b -> p a b -> m ()
l %= f = modify' (l %~ f)
{-# INLINE (%=) #-}
infix 4 %=

-- | (+=) using strict modify'
(+=) :: (Num a, MonadState s m) => ASetter s s a a -> a -> m ()
l += x = modify' (l +~ x)
{-# INLINE (+=) #-}
infix 4 +=

-- | (-=) using strict modify'
(-=) :: (Num a, MonadState s m) => ASetter s s a a -> a -> m ()
l -= x = modify' (l -~ x)
{-# INLINE (-=) #-}
infix 4 -=

-- | (*=) using strict modify'
(*=) :: (Num a, MonadState s m) => ASetter s s a a -> a -> m ()
l *= x = modify' (l *~ x)
{-# INLINE (*=) #-}
infix 4 *=

-- | Restricts an action to MonadReader from within MonadState
immutably :: MonadState s m => ReaderT s m a -> m a
immutably a = runReaderT a =<< get

-- | Allows using a MonadState function that just uses "get" from within MonadReader
statefully :: MonadReader s m => StateT s m a -> m a
statefully a = evalStateT a =<< ask
