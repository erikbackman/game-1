{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Game1 where

import Control.Lens (use, (%=))
import Control.Lens.Lens
import Control.Monad.RWS
import Control.Monad.Reader
  ( ReaderT (ReaderT, runReaderT),
  )
import Control.Monad.State
  ( StateT,
    evalStateT,
  )
import Game1.GameState
  ( GameState (..),
    gs_map,
    gs_player,
    gs_running,
    initGameState,
    parseMap, PlayerState (Walking), playerState, playerPos
  )
import Game1.Input
  ( Intent (Idle, Move, Quit),
    inputToIntent,
    pollEventPayloads,
  )
import Game1.Map (Map)
import Game1.Player
  ( move
  )
import Game1.Resources
  ( Resources (..),
    withResources,
  )
import Game1.Scene (drawScene)
import Game1.Window (withWindow)
import qualified SDL
import qualified SDL.Time as Time

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  gameMap <- loadMap "assets/map.txt"

  case gameMap of
    Nothing -> SDL.quit
    Just m -> do
      withWindow
        "Game1"
        SDL.defaultWindow
        \w -> withResources w \r ->
          runGame1 r (initGameState m r) mainLoop
      SDL.quit

newtype Game1 a
  = Game1 (ReaderT Resources (StateT GameState IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Resources, MonadState GameState, MonadIO)

runGame1 :: Resources -> GameState -> Game1 a -> IO a
runGame1 r s (Game1 m) = evalStateT (runReaderT m r) s

loadMap :: MonadIO m => FilePath -> m (Maybe Map)
loadMap = liftIO . (readFile >=> pure . parseMap)

whileState :: Monad m => MonadState s m => Lens' s Bool -> m () -> m ()
whileState cond act = do
  b <- use cond
  when b $ do
    act
    whileState cond act

update :: (MonadIO m, MonadState GameState m) => m ()
update = do
  input <- pollEventPayloads
  case inputToIntent input of
    Quit -> gs_running #= False
    Idle -> pure ()
    Move dir_vec -> do
      m <- use gs_map
      gs_player %= move m dir_vec

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
  whileState gs_running $ do
    fstart <- Time.ticks
    update
    render

    ftime <- fmap (fstart -) Time.ticks
    when (fdelay > ftime) $ Time.delay (fdelay - ftime)
