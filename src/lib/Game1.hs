{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Game1 where

import           Control.Concurrent             ( threadDelay )
import           Control.Lens                   ( (%=) )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(ReaderT, runReaderT)
                                                , asks
                                                )
import           Control.Monad.State            ( MonadState
                                                , StateT
                                                , evalStateT
                                                )
import           Game1.GameState                ( GameState(..)
                                                , playerPos
                                                )
import           Game1.Input                    ( Intent(Idle, Move, Quit)
                                                , inputToIntent
                                                , pollEventPayloads
                                                )
import           Game1.Player                   ( nextPlayerPos
                                                , startPosition
                                                )
import           Game1.Resources                ( Resources(..)
                                                , withResources
                                                )
import           Game1.Window                   ( withWindow )

import           Game1.Scene                    ( drawScene )

import qualified SDL

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  withWindow "Game1"
             windowConfig
             \w -> withResources w \r -> runGame1 r initState mainLoop
  SDL.quit

 where
  initState    = GameState { _playerPos = startPosition }
  windowConfig = SDL.defaultWindow

newtype Game1 a =
  Game1 (ReaderT Resources (StateT GameState IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Resources, MonadState GameState, MonadIO)

runGame1 :: Resources -> GameState -> Game1 a -> IO a
runGame1 r s (Game1 m) = evalStateT (runReaderT m r) s

mainLoop
  :: (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
mainLoop = do
  input <- pollEventPayloads
  case inputToIntent input of
    Quit       -> pure ()
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
