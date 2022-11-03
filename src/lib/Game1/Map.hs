module Game1.Map where

import Control.Monad.RWS
import Foreign.C
import Game1.Resources
import SDL hiding (Unknown)
import qualified Data.Map.Strict as SMap
import Data.Map.Strict ((!?))

type Map a = SMap.Map (V2 Int) a

data Tile = Tile
  { tileType :: TileType,
    tylePos :: V2 CInt
  }

data TileType = Solid Char | Empty | Unknown deriving (Eq, Show)

tileToTexture :: MonadReader Resources m => Char -> m Texture
tileToTexture t = useResources $ \r -> pure $
  fst $ case t of
    '|' -> tex_pillar r
    '#' -> tex_black r
    '.' -> tex_tile r
    '?' -> tex_black r
    _   -> tex_tile r

getTileType :: V2 Int -> Map Char -> TileType
getTileType pos m =
  case m !? pos of -- TODO: use ix lenses!
    Just '#' -> Solid '#'
    Just '|' -> Solid '#'
    Just '?' -> Unknown
    Just '.' -> Empty
    _        -> Empty
