module Game1.Window where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Foreign.C (CInt)
import SDL (Point (P), Rectangle (Rectangle), V2 (V2))
import qualified SDL

windowSize :: V2 CInt
windowSize = V2 1280 720

outOfBounds :: Rectangle CInt -> Bool
outOfBounds (Rectangle (P (V2 x y)) (V2 w h)) =
  let V2 ww wh = windowSize
   in x <= 0 || x + w > ww || y <= 0 || y + h > wh

withWindow :: MonadIO m => Text -> SDL.WindowConfig -> (SDL.Window -> m a) -> m ()
withWindow title config f = do
  w <- SDL.createWindow title (config {SDL.windowInitialSize = windowSize})
  f w
  SDL.destroyWindow w
