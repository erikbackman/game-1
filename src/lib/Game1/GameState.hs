{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game1.GameState where

import Control.Lens (makeLenses)
import Game1.Resources (Resources (..))
import Linear.V2 (V2 (V2))
import Text.Read (readMaybe)
import Game1.Map (Map, tileToTexture)
import Control.Monad.RWS
import Game1.Render
import Control.Lens.Getter (use)

data PlayerState = Idle | Walking
  deriving Show

data Player = Player
  { _playerPos :: V2 Int,
    _playerSpeed :: Int,
    _playerDir :: V2 Int,
    _playerState :: PlayerState
  }
  deriving (Show)

data GameState = GameState
  { _gs_player :: Player,
    _gs_map :: Map,
    _gs_running :: Bool
  }
  deriving stock (Show)

makeLenses ''Player
makeLenses ''GameState

parseMap :: String -> Maybe Map
parseMap = (traverse . traverse) readMaybe . fmap words . lines

drawMap :: (MonadIO m, MonadState GameState m, MonadReader Resources m) => Map -> m ()
drawMap m = do
  dir <- use (gs_player . playerDir) 
  v <- use (gs_player . playerPos)
  let V2 v1 v2 = v
      tiles = [ (tile, V2 (fromIntegral x) (fromIntegral y))
              | (y, row) <- enumerate m,
                (x, tile) <- enumerate row
              ]
  mapM_ renderTile tiles
  where
    enumerate = zip [0..]
    enumerateFromTo f t = zip [0..t] . drop f
    renderTile (t, p) = do
      tex <- tileToTexture t
      renderTexture tex p (V2 False False)

startPosition :: Num n => V2 n
startPosition = V2 1 1

initGameState :: Map -> Resources -> GameState
initGameState m Resources {tex_player} =
  GameState
    { _gs_player = Player startPosition 1 (V2 0 0) Idle,
      _gs_map = m,
      _gs_running = True
    }
