module Game1.Resources where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Paths_game1

import qualified SDL
import qualified SDL.Image

data Resources = Resources
  { tex_box      :: SDL.Texture
  , sdl_renderer :: SDL.Renderer
  }

loadResources :: SDL.Window -> IO Resources
loadResources window = do
  sdl_renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  tex_box <- liftIO $ getDataFileName "assets/player.png" >>= loadTexture sdl_renderer
  pure $ Resources { .. }
  where
    loadTexture renderer path = SDL.Image.loadTexture renderer path

destroyResources :: Resources -> IO ()
destroyResources Resources
  { tex_box
  , sdl_renderer
  } = do

  SDL.destroyTexture tex_box
  SDL.destroyRenderer sdl_renderer
