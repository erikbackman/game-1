module Game1.Window where

import Control.Monad.IO.Class (MonadIO)
import SDL (V2(V2))

import qualified SDL
import Data.Text (Text)

withWindow :: MonadIO m => Text -> SDL.WindowConfig -> (SDL.Window -> m a) -> m ()
withWindow title config f = do
  w <- SDL.createWindow title config
  f w
  SDL.destroyWindow w
