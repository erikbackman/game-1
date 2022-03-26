{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Game1.GameState where

import Control.Lens (makeLenses)
import Control.Monad.RWS
import Foreign
import Foreign.C (CInt)
import Game1.Render (renderTexture)
import Game1.Resources (Resources (..), useResources)
import Linear.V2 (V2 (V2))
import SDL (Texture)
import Text.Read (readMaybe)

type Entity = Char

type Map = [[Int]]

data Tile = Tile
  { tileType :: TileType,
    tylePos :: V2 CInt
  }

data TileType = Solid Int | Empty deriving (Eq, Show)

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

getTileType :: V2 Int -> Map -> TileType
getTileType (V2 x y) m =
  let i = m !! x !! y
   in case i of
        3 -> Solid 3
        2 -> Solid 2
        _ -> Empty

tileToTexture :: MonadReader Resources m => Int -> m Texture
tileToTexture t = useResources $ \r -> pure $
  fst $ case t of
    2 -> tex_pillar r
    3 -> tex_tile r
    _ -> tex_tile r

drawMap :: (MonadIO m, MonadReader Resources m) => Map -> m ()
drawMap m =
  mapM_
    renderTile
    [ (tile, V2 (fromIntegral x) (fromIntegral y))
      | (y, row) <- enumerate m,
        (x, tile) <- enumerate row
    ]
  where
    enumerate = zip [0 ..]
    renderTile (t, p) = do
      tex <- tileToTexture t
      let targetPos = 32 * p
      renderTexture tex targetPos

startPosition :: V2 Int
startPosition = V2 1 1

initGameState :: Map -> Resources -> GameState
initGameState m Resources {tex_player} =
  GameState
    { _gsPlayer = Player startPosition 1,
      _gsMap = m,
      _gsRunning = True
    }
