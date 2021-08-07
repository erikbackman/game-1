module Game1.Window where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Foreign.C (CInt)
import SDL (Point (P), V2 (V2))
import qualified SDL

windowSize :: V2 CInt
windowSize = V2 1280 720

withinBounds :: Point V2 CInt -> Bool
withinBounds (P (V2 x y)) =
  let V2 w h = windowSize
   in x > 0 && y > 0 && x < w && y < h

withWindow :: MonadIO m => Text -> SDL.WindowConfig -> (SDL.Window -> m a) -> m ()
withWindow title config f = do
  w <- SDL.createWindow title (config {SDL.windowInitialSize = windowSize})
  f w
  SDL.destroyWindow w
