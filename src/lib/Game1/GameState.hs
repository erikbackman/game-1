{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Game1.GameState where

import Control.Lens (makeLenses)
import Control.Lens.Getter (use)
import Control.Monad.RWS
import Game1.Map (Map, tileToTexture)
import Game1.Render
import Game1.Resources (Resources (..))
import Linear.V2 (V2 (V2))
import Text.Read (readMaybe)
import qualified Data.Map.Strict as SMap

data PlayerState = Idle | Walking
  deriving (Show)

type GameStateM m = MonadState GameState m

type GameResourcesM m = MonadReader Resources m

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
parseMap str =
  let x = (traverse . traverse) readMaybe . fmap words . lines $ str
  in fmap toMap x
  where
    toMap m = SMap.fromList $ [ (V2 (fromIntegral x) (fromIntegral y), tile)
                              | (y, row) <- enumerate m,
                                (x, tile) <- enumerate row
                              ]
    enumerate = zip [0..]

renderTile :: (MonadIO m, GameResourcesM m) => (V2 Int, Int) -> m ()
renderTile (pos, txId) = renderTexture pos =<< tileToTexture txId

drawMap :: (MonadIO m, GameStateM m, GameResourcesM m) => Map -> m ()
drawMap = mapM_ renderTile . SMap.toList

startPosition :: Num n => V2 n
startPosition = V2 1 1

initGameState :: Map -> Resources -> GameState
initGameState m Resources {tex_player} =
  GameState
    { _gs_player = Player startPosition 1 (V2 0 0) Idle,
      _gs_map = m,
      _gs_running = True
    }
