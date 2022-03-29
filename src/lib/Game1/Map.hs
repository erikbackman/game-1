module Game1.Map where

import Control.Monad.RWS
import Foreign.C
import Game1.Resources
import SDL

type Map = [[Int]]

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
getTileType (V2 x y) m =
  -- use ix lenses!
  case m !! x !! y of
    3 -> Solid 3
    2 -> Solid 2
    _ -> Empty
