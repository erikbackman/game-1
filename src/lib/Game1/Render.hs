module Game1.Render where

import Foreign.C (CInt, CDouble)
import SDL (V2 (V2))
import qualified SDL
import Control.Monad.RWS
import Game1.Resources

renderTexture ::
  MonadIO m => MonadReader Resources m => SDL.Texture -> V2 CInt -> V2 Bool -> m ()
renderTexture tex pos rot = do
  ti <- SDL.queryTexture tex
  renderer <- asks sdl_renderer
  let tsize = V2 (SDL.textureWidth ti) (SDL.textureHeight ti)
  SDL.copyEx renderer tex Nothing (Just $ SDL.Rectangle (SDL.P pos) tsize) 0 Nothing rot

withRenderer :: MonadReader Resources m => (SDL.Renderer -> m a) -> m a
withRenderer f = asks sdl_renderer >>= f
