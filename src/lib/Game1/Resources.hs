module Game1.Resources (Resources (..), withResources, useResources) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Paths_game1
import qualified SDL
import qualified SDL.Image
import Control.Monad.RWS
import Control.Monad

data Resources = Resources
  { tex_player :: (SDL.Texture, SDL.TextureInfo),
    tex_enemy :: (SDL.Texture, SDL.TextureInfo),
    tex_tile :: (SDL.Texture, SDL.TextureInfo),
    tex_pillar :: (SDL.Texture, SDL.TextureInfo),
    sdl_renderer :: SDL.Renderer
  }

loadResources :: SDL.Window -> IO Resources
loadResources window = do
  sdl_renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let loadAsset = loadAssetWithRenderer sdl_renderer
  tex_player <- loadAsset "knight.png"
  tex_enemy <- loadAsset "enemy.png"
  tex_tile <- loadAsset "tile.png"
  tex_pillar <- loadAsset "pillar.png"
  pure $ Resources {..}
  where
    loadTexture renderer path = SDL.Image.loadTexture renderer path
    loadAssetWithRenderer r name = do
      path <- liftIO $ getDataFileName ("assets/" <> name)
      tex <- loadTexture r path
      ti <- SDL.queryTexture tex
      pure (tex, ti)

withResources :: SDL.Window -> (Resources -> IO ()) -> IO ()
withResources window f = do
  resources <- loadResources window
  f resources
  destroyResources resources

useResources :: MonadReader Resources m => (Resources -> m a) -> m a
useResources f = ask >>= f

destroyResources :: Resources -> IO ()
destroyResources
  Resources
    { tex_player,
      tex_enemy,
      tex_tile,
      sdl_renderer
    } = do
    SDL.destroyTexture $ fst tex_player
    SDL.destroyTexture $ fst tex_enemy
    SDL.destroyTexture $ fst tex_tile
    SDL.destroyRenderer sdl_renderer
