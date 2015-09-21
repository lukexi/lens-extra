module Control.Lens.Extra 
  ( module Extra
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

l .= x = modify' (l .~ x)
{-# INLINE (.=) #-}
infix 4 .=

l ?= x = modify' (l ?~ x)
{-# INLINE (?=) #-}
infix 4 ?=

l %= f = modify' (l %~ f)
{-# INLINE (%=) #-}
infix 4 %=

l += x = modify' (l +~ x)
{-# INLINE (+=) #-}
infix 4 +=

l -= x = modify' (l -~ x)
{-# INLINE (-=) #-}
infix 4 -=

l *= x = modify' (l *~ x)
{-# INLINE (*=) #-}
infix 4 *=

