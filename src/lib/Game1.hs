{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Game1 where

import Control.Lens (use, (%=))
import Control.Monad.Reader
  ( ReaderT (ReaderT, runReaderT),
  )
import Control.Monad.State
  ( StateT,
    evalStateT,
  )
import Game1.GameState
  ( GameState (..),
    initGameState, ppos, player, running, _ppos, _player
  )
import Game1.Input
  ( Intent (Idle, Move, Quit),
    inputToIntent,
    pollEventPayloads,
  )
import Game1.Player
  ( nextPlayerPos,
  )
import Game1.Resources
  ( Resources (..),
    withResources,
  )
import Game1.Scene (drawScene)
import Game1.Window (withWindow)
import qualified SDL
import qualified SDL.Time as Time
import Control.Monad.RWS
import Control.Lens.Lens

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  withWindow
    "Game1"
    SDL.defaultWindow
    \w -> withResources w \r ->
      runGame1 r (initGameState r) mainLoop
  SDL.quit

newtype Game1 a
  = Game1 (ReaderT Resources (StateT GameState IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Resources, MonadState GameState, MonadIO)

runGame1 :: Resources -> GameState -> Game1 a -> IO a
runGame1 r s (Game1 m) = evalStateT (runReaderT m r) s

whileState :: Monad m => MonadState s m => Lens' s Bool -> m () -> m ()
whileState p f = do
  b <- use p
  when b $ do
   f
   whileState p f

update :: (MonadIO m, MonadState GameState m) => m ()
update = do
  input <- pollEventPayloads
  case inputToIntent input of
    Quit -> running #= False
    Idle -> pure ()
    Move delta -> do
      state <- use id
      liftIO $ print delta
      player.ppos %= nextPlayerPos (_map state) delta

render :: (MonadIO m, MonadState GameState m, MonadReader Resources m) => m ()
render = do
  r <- asks sdl_renderer
  SDL.clear r
  drawScene
  SDL.present r

mainLoop ::
  (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
mainLoop = do
  let fdelay = 1000 `div` 60
  whileState running $ do
    fstart <- Time.ticks
    gs <- get

    update
    liftIO $ print gs

    render
    ftime <- fmap (fstart -) Time.ticks
    when (fdelay > ftime) $ do
      Time.delay (fdelay - ftime)
