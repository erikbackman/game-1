module Game1.Map where

import Control.Monad.RWS
import Foreign.C
import Game1.Render
import Game1.Resources
import SDL

type Map = [[Int]]

data Tile = Tile
  { tileType :: TileType,
    tylePos :: V2 CInt
  }

data TileType = Solid Int | Empty deriving (Eq, Show)

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
      renderTexture tex targetPos (V2 False False)

tileToTexture :: MonadReader Resources m => Int -> m Texture
tileToTexture t = useResources $ \r -> pure $
  fst $ case t of
    2 -> tex_pillar r
    3 -> tex_tile r
    _ -> tex_tile r

getTileType :: V2 Int -> Map -> TileType
getTileType (V2 x y) m =
  let i = m !! x !! y
   in case i of
        3 -> Solid 3
        2 -> Solid 2
        _ -> Empty
