module Game1.Render where

import Control.Monad.RWS
import Foreign.C.Types
import Game1.Resources
import Linear (M22, (!*))
import SDL (V2 (V2))
import qualified SDL

renderTextureWithRot :: (MonadIO m, MonadReader Resources m, Integral a)
  => V2 a -> V2 Bool -> SDL.Texture -> m ()
renderTextureWithRot posv rot tex = do
  tex_info <- SDL.queryTexture tex
  renderer <- asks sdl_renderer
  
  let tsize  = V2 (SDL.textureWidth tex_info) (SDL.textureHeight tex_info)
      tposv  = renderBasis !* (toCInt <$> posv)
      src    = Nothing
      dest   = Just $ SDL.Rectangle (SDL.P tposv) tsize
      angle  = 0
      center = Nothing
      
  SDL.copyEx renderer tex src dest angle center rot

renderTexture :: (MonadIO m, MonadReader Resources m, Integral a)
  => V2 a -> SDL.Texture -> m ()
renderTexture posv = renderTextureWithRot posv (V2 False False)

withRenderer :: MonadReader Resources m => (SDL.Renderer -> m a) -> m a
withRenderer f = f =<< asks sdl_renderer

renderBasis :: M22 CInt
renderBasis = V2 (V2 32 0) (V2 0 32)

toCInt :: Integral a => a -> CInt
toCInt = CInt . fromIntegral
