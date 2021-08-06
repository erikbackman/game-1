module Game1.Resources where

import           Paths_game1
import           Control.Monad.IO.Class (MonadIO(liftIO))

import qualified SDL
import qualified SDL.Image

data Resources = Resources
  { tex_box :: SDL.Texture
  }

loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
  tex_box <- liftIO $ getDataFileName "assets/box.png" >>= loadTexture renderer
  pure $ Resources { .. }
  where
    loadTexture renderer path = SDL.Image.loadTexture renderer path
