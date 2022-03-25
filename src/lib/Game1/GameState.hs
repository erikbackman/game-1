{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Game1.GameState where

import Control.Lens (makeLenses)
import Foreign.C (CInt)
import Game1.Resources (Resources (..))
import Linear.V2 (V2 (V2))
import qualified SDL
import Control.Monad.RWS
import Game1.Render (renderTexture)

type Entity = Char

type Map = [[Int]]

data Tile = Solid | Empty deriving (Eq, Show)

gameMap :: Map
gameMap = [
  [ 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3 ],
  [ 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 ]
  ]

getTile :: (Int, Int) -> Map -> Tile
getTile (x,y) m =
  case m !! x !! y of
       3 -> Solid
       _ -> Empty

cintPoint :: (Int, Int) -> V2 CInt
cintPoint (x,y) = V2 (fromIntegral x) (fromIntegral y)

drawMap :: (MonadIO m, MonadReader Resources m) => Map -> m ()
drawMap m = do
  r <- asks sdl_renderer
  (tx, _) <- asks tex_tile
  
  let pts = [ cintPoint (x*32,y*32) | (y, row) <- enumerate m, (x, til) <- enumerate row ]
  forM_ pts (renderTexture r tx)
  where
    enumerate = zip [0..]

newtype Player = Player
  { _ppos :: V2 Int
  } deriving (Show)
  
data GameState = GameState
  { _player :: Player,
    _map :: Map,
    _running :: Bool
  }
  deriving stock (Show)

startPosition :: V2 Int
startPosition = V2 1 1

initGameState :: Resources -> GameState
initGameState Resources {tex_player} =
  let ti = snd tex_player
      (pw, ph) = (SDL.textureWidth ti, SDL.textureHeight ti)
      pr = Player startPosition 
   in GameState { _player = pr
                , _map = gameMap
                , _running = True
                }

makeLenses ''Player
makeLenses ''GameState
