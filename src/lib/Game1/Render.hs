module Game1.Render where

import Foreign.C (CInt)
import SDL (V2 (V2))
import qualified SDL
import Control.Monad.RWS
import Game1.Resources

renderTexture ::
  MonadIO m => MonadReader Resources m => SDL.Texture -> V2 CInt -> m ()
renderTexture tex pos = do
  ti <- SDL.queryTexture tex
  renderer <- asks sdl_renderer
  let tsize = V2 (SDL.textureWidth ti) (SDL.textureHeight ti)
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle (SDL.P pos) tsize)

withRenderer :: MonadReader Resources m => (SDL.Renderer -> m a) -> m a
withRenderer f = asks sdl_renderer >>= f
