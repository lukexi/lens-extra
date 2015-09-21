import Control.Lens.Extra
import Control.Monad.State

-- This should not leak with our modified combinators
main :: IO ()
main = 
  void . flip runStateT Map.empty $ do
    forM_ [0..1000000] $ \i -> do
      at "0" ?== show (i::Int)
      forceState
    liftIO . print =<< use id
