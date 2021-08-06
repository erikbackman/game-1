module Game1.Window where

import Control.Monad.IO.Class (MonadIO)
import SDL (V2(V2))

import qualified SDL

withWindow :: MonadIO m => (SDL.Window -> m a) -> m ()
withWindow f = do
  w <- SDL.createWindow "Game1" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  f w
  SDL.destroyWindow w
