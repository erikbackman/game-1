module Game1.Resources (Resources (..), withResources) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Paths_game1
import qualified SDL
import qualified SDL.Image

data Resources = Resources
  { tex_player :: SDL.Texture,
    tex_tile :: SDL.Texture,
    sdl_renderer :: SDL.Renderer
  }

loadResources :: SDL.Window -> IO Resources
loadResources window = do
  sdl_renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let loadAsset = loadAssetWithRenderer sdl_renderer
  tex_player <- loadAsset "player.png"
  tex_tile <- loadAsset "tile.png"
  pure $ Resources {..}
  where
    loadTexture renderer path = SDL.Image.loadTexture renderer path
    loadAssetWithRenderer r name = liftIO $ getDataFileName ("assets/" <> name) >>= loadTexture r

withResources :: SDL.Window -> (Resources -> IO ()) -> IO ()
withResources window f = do
  resources <- loadResources window
  f resources
  destroyResources resources

destroyResources :: Resources -> IO ()
destroyResources
  Resources
    { tex_player,
      tex_tile,
      sdl_renderer
    } = do
    SDL.destroyTexture tex_player
    SDL.destroyTexture tex_tile
    SDL.destroyRenderer sdl_renderer
