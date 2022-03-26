module Game1.Render where

import Control.Monad.IO.Class (MonadIO)
import Foreign.C (CInt)
import SDL (Point, V2 (V2))
import qualified SDL
import Control.Monad.RWS
import Game1.Resources

renderTexture ::
  MonadIO m => MonadReader Resources m => SDL.Texture -> V2 CInt -> m ()
renderTexture tex pos = do
  ti <- SDL.queryTexture tex
  renderer <- asks sdl_renderer
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      extent = V2 w h

  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle (SDL.P pos) extent)

--withRenderer :: MonadResources
