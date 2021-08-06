module Game1.Render where

import Control.Monad.IO.Class (MonadIO)
import SDL
import Foreign.C (CInt(CInt))
import Control.Monad.Reader.Class (MonadReader, asks)

import Game1.Resources

import qualified SDL

renderTexture
  :: MonadIO m => SDL.Renderer -> SDL.Texture -> (Point V2 CInt) -> m ()
renderTexture renderer tex pos = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      extent = V2 w h
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle pos extent)

renderPlayer :: (MonadIO m, MonadReader Resources m) => Point V2 CInt -> m ()
renderPlayer pos = do
  renderer <- asks sdl_renderer
  image    <- asks tex_box
  renderTexture renderer image pos
