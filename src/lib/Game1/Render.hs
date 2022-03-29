{-# LANGUAGE MultiParamTypeClasses #-}

module Game1.Render where

import Control.Monad.RWS
import Foreign.C.Types
import Game1.Resources
import Linear (M22, (!*))
import SDL (V2 (V2))
import qualified SDL

renderTexture ::
  (MonadIO m, MonadReader Resources m, Integral a) => SDL.Texture -> V2 a -> V2 Bool -> m ()
renderTexture tex posv rot = do
  ti <- SDL.queryTexture tex
  renderer <- asks sdl_renderer
  let tsize = V2 (SDL.textureWidth ti) (SDL.textureHeight ti)
      target = renderBasis !* (toCInt <$> posv) :: V2 CInt
  SDL.copyEx renderer tex Nothing (Just $ SDL.Rectangle (SDL.P target) tsize) 0 Nothing rot

withRenderer :: MonadReader Resources m => (SDL.Renderer -> m a) -> m a
withRenderer f = asks sdl_renderer >>= f

renderBasis :: M22 CInt
renderBasis = V2 (V2 32 0) (V2 0 32)

toCInt :: Integral a => a -> CInt
toCInt = CInt . fromIntegral
