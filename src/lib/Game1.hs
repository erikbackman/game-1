{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Game1 where

import           Control.Concurrent     (threadDelay)
import           Control.Lens           ((%=), (%~), (+=), (.=), (^.))
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
import           SDL                    (Point (P), Renderer, V2 (V2), _x, _y,
                                         ($=))

import           Game1.GameState        (GameState (..), playerPos)
import           Game1.Input            (Intent (Idle, Move, Quit),
                                         inputToIntent, pollEventPayloads)
import           Game1.Player           (nextPlayerPos, renderPlayer,
                                         startPosition)
import           Game1.Resources        (Resources (..), destroyResources,
                                         loadResources)
import           Game1.Window           (withWindow)

import qualified SDL
import qualified SDL.Image
import Game1.Scene (drawScene)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  withWindow "Game1" windowConfig \w -> do
    resources <- loadResources w
    runGame1 resources initState mainLoop
    destroyResources resources

  where
    initState = GameState { _playerPos = startPosition }
    windowConfig = SDL.defaultWindow

newtype Game1 a =
  Game1 (ReaderT Resources (StateT GameState IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Resources, MonadState GameState, MonadIO)

runGame1 :: Resources -> GameState -> Game1 a -> IO a
runGame1 r s (Game1 m) = evalStateT (runReaderT m r) s

mainLoop :: (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
mainLoop = do
  input <- pollEventPayloads
  case inputToIntent input of
    Quit       -> SDL.quit
    Idle       -> step
    Move delta -> playerPos %= nextPlayerPos delta >> step

  where
    step = do
      r <- asks sdl_renderer
      SDL.clear r
      drawScene
      SDL.present r
      liftIO $ threadDelay (1000 * 16)
      mainLoop
