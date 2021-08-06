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
import           Game1.Render           (renderPlayer)
import           Game1.Resources        (Resources (..), destroyResources,
                                         loadResources)
import           Game1.Window           (withWindow)

import qualified SDL
import qualified SDL.Image

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  withWindow "Game1" windowConfig \w -> do
    resources <- loadResources w
    runGame1 resources initState mainLoop
    destroyResources resources

  where
    initState = GameState { _playerPos = startPosition }
    windowConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }

newtype Game1 a =
  Game1 (ReaderT Resources (StateT GameState IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Resources, MonadState GameState, MonadIO)

runGame1 :: Resources -> GameState -> Game1 a -> IO a
runGame1 r s (Game1 m) = evalStateT (runReaderT m r) s

startPosition :: Point V2 CInt
startPosition = P $ V2 100 100

mainLoop :: (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
mainLoop = do
  input <- pollEventPayloads
  case inputToIntent input of
    Quit       -> SDL.quit
    Idle       -> step
    Move delta -> playerPos += P delta >> step

  where
    step = do
      r <- asks sdl_renderer
      p <- gets _playerPos
      SDL.clear r
      renderPlayer p
      SDL.present r
      liftIO $ threadDelay (1000 * 16)
      mainLoop
