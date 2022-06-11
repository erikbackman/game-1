module Game1.Map where

import Control.Monad.RWS
import Foreign.C
import Game1.Resources
import SDL
import qualified Data.Map.Strict as SMap
import Data.Map.Strict ((!?))

type Map = SMap.Map (V2 Int) Int

data Tile = Tile
  { tileType :: TileType,
    tylePos :: V2 CInt
  }

data TileType = Solid Int | Empty deriving (Eq, Show)

tileToTexture :: MonadReader Resources m => Int -> m Texture
tileToTexture t = useResources $ \r -> pure $
  fst $ case t of
    2 -> tex_pillar r
    3 -> tex_tile r
    _ -> tex_tile r

getTileType :: V2 Int -> Map -> TileType
getTileType pos m =
  case m !? pos of -- TODO: use ix lenses!
    Just 3 -> Solid 3
    Just 2 -> Solid 2
    _      -> Empty

data Creature = Enemy1 | Enemy2

spawnCreature :: Creature -> V2 Int -> Map -> Map
spawnCreature _ (V2 x y) = undefined 
