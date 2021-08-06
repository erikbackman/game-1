{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Game1 where

import           Control.Concurrent     (threadDelay)
import           Control.Lens           (withIndex, (+=))
import           Control.Monad          (void)
import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader (..),
                                         ReaderT (ReaderT, runReaderT), asks,
                                         reader, runReader)
import           Control.Monad.State    (MonadIO (..), MonadState, StateT,
                                         evalStateT, gets)
import           Data.Foldable          (foldl')
import           Data.Monoid            (Any (Any))
import           Data.Semigroup         (Sum (Sum))
import           Data.Text              (Text)
import           Foreign.C              (CInt)
import           Paths_game1            ()
import           SDL                    (Point (P), Renderer, V2 (V2), ($=))

import           Game1.GameState        (GameState (..), playerPos)
import           Game1.Input
import           Game1.Resources        (Resources (..), destroyResources,
                                         loadResources)

import qualified SDL
import qualified SDL.Image

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  withWindow \w -> do
    resources <- loadResources w

    let
      initState =
        GameState { _playerPos = startPosition }

    runGame1 resources initState mainLoop
    destroyResources resources

withWindow :: MonadIO m => (SDL.Window -> m a) -> m ()
withWindow f = do
  w <- SDL.createWindow "Game1" SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  f w
  SDL.destroyWindow w

startPosition :: Point V2 CInt
startPosition = P $ V2 100 100

newtype Game1 a =
  Game1 (ReaderT Resources (StateT GameState IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Resources, MonadState GameState, MonadIO)

runGame1 :: Resources -> GameState -> Game1 a -> IO a
runGame1 r s (Game1 m) = evalStateT (runReaderT m r) s

renderTexture
  :: MonadIO m => SDL.Renderer -> SDL.Texture -> (Point V2 CInt) -> m ()
renderTexture renderer tex pos = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      extent = V2 w h
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle pos extent)

pollEventPayloads :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads = liftIO $ fmap SDL.eventPayload <$> SDL.pollEvents

renderPlayer :: (MonadIO m, MonadReader Resources m) => Point V2 CInt -> m ()
renderPlayer pos = do
  renderer <- asks sdl_renderer
  image    <- asks tex_box
  renderTexture renderer image pos

mainLoop :: (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
mainLoop = do
  renderer <- asks sdl_renderer
  loop renderer
 where
  loop r = do
    input <- pollEventPayloads
    case inputToIntent input of
      Quit       -> SDL.quit
      Idle       -> step r
      Move delta -> playerPos += P delta >> step r

  step r' = do
    p <- gets _playerPos
    SDL.clear r'
    renderPlayer p
    SDL.present r'
    liftIO $ threadDelay (1000 * 16)
    loop r'
