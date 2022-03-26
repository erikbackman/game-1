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
    _playerSpeed :: Int
  }
  deriving (Show)

data GameState = GameState
  { _gsPlayer :: Player,
    _gsMap :: Map,
    _gsRunning :: Bool
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
    { _gsPlayer = Player startPosition 1,
      _gsMap = m,
      _gsRunning = True
    }
