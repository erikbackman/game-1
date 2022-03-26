{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Game1.GameState where

import Control.Lens (makeLenses)
import Game1.Resources (Resources (..))
import Linear.V2 (V2 (V2))
import Text.Read (readMaybe)
import Game1.Map (Map)

type Entity = Char

data Player = Player
  { _playerPos :: V2 Int,
    _playerSpeed :: Int,
    _playerDir :: V2 Int
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

startPosition :: V2 Int
startPosition = V2 1 1

initGameState :: Map -> Resources -> GameState
initGameState m Resources {tex_player} =
  GameState
    { _gs_player = Player startPosition 1 (V2 0 0),
      _gs_map = m,
      _gs_running = True
    }
