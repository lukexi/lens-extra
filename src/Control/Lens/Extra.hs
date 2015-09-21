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

