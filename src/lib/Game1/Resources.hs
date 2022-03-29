{-# LANGUAGE DeriveDataTypeable #-}

module Game1.Resources (Resources (..), withResources, useResources) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Paths_game1
import qualified SDL
import qualified SDL.Image
import Control.Monad.RWS
import Control.Monad
import Data.Data

data Resources = Resources
  { tex_player :: (SDL.Texture, SDL.TextureInfo),
    tex_player2 :: (SDL.Texture, SDL.TextureInfo),
    tex_enemy :: (SDL.Texture, SDL.TextureInfo),
    tex_tile :: (SDL.Texture, SDL.TextureInfo),
    tex_pillar :: (SDL.Texture, SDL.TextureInfo),
    tex_heart :: (SDL.Texture, SDL.TextureInfo),
    sdl_renderer :: SDL.Renderer
  }

loadResources :: SDL.Window -> IO Resources
loadResources window = do
  sdl_renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let loadAsset = loadAssetWithRenderer sdl_renderer
  tex_player <- loadAsset "knight.png"
  tex_player2 <- loadAsset "knight2.png" 
  tex_enemy <- loadAsset "enemy.png"
  tex_tile <- loadAsset "tile.png"
  tex_pillar <- loadAsset "pillar.png"
  tex_heart <- loadAsset "heart.png"
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
      tex_player2,
      tex_enemy,
      tex_tile,
      tex_pillar,
      tex_heart,
      sdl_renderer
    } = do
    SDL.destroyTexture $ fst tex_player
    SDL.destroyTexture $ fst tex_player2
    SDL.destroyTexture $ fst tex_enemy
    SDL.destroyTexture $ fst tex_tile
    SDL.destroyTexture $ fst tex_pillar
    SDL.destroyTexture $ fst tex_heart  
    SDL.destroyRenderer sdl_renderer
