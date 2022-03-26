{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Game1.GameState where

import Foreign.C (CInt)
import Game1.Resources (Resources (..), useResources)
import Linear.V2 (V2 (V2))
import qualified SDL
import Control.Monad.RWS
import Game1.Render (renderTexture)
import SDL (Texture)
import Control.Lens (makeLenses)
import Text.Read (readMaybe)

type Entity = Char

type Map = [[Int]]

data Tile = Solid Int | Empty deriving (Eq, Show)

parseMap :: String -> Maybe Map
parseMap = (traverse . traverse) readMaybe . fmap words . lines

getTile :: V2 Int -> Map -> Tile
getTile (V2 x y) m =
  case m !! x !! y of
       3 -> Solid 3
       2 -> Solid 2
       _ -> Empty

tileToTexture :: MonadReader Resources m => Tile -> m Texture
tileToTexture t = useResources $ \r -> pure $ fst $ case t of
  Solid 2 -> tex_pillar r
  Solid 3 -> tex_tile r
  _       -> tex_tile r 

cintPoint :: (Int, Int) -> V2 CInt
cintPoint (x,y) = V2 (fromIntegral x) (fromIntegral y)

drawMap :: (MonadIO m, MonadReader Resources m) => Map -> m ()
drawMap m = do
  let
    renderTile p = do
      let
        t = getTile (fmap fromIntegral p) m
        targetPos = p*32
      tex <- tileToTexture t
      renderTexture tex targetPos
      
    enumerate = zip [0..]
    pts = [ cintPoint (x, y) | (y, row) <- enumerate m, (x, til) <- enumerate row ]
    
  forM_ pts renderTile

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

initGameState :: Map -> Resources -> GameState
initGameState m Resources {tex_player} =
  let ti = snd tex_player
      (pw, ph) = (SDL.textureWidth ti, SDL.textureHeight ti)
      pr = Player startPosition 
   in GameState { _player = pr
                , _map = m
                , _running = True
                }

makeLenses ''Player
makeLenses ''GameState
