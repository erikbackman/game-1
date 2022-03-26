module Game1.Player where

import Control.Lens.Setter ((.~))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
  ( MonadReader,
    asks,
  )
import Data.Function ((&))
import Foreign.C (CInt)
import Foreign.C.Types (CInt (CInt))
import Game1.GameState
import Game1.Render (renderTexture)
import Game1.Resources
  ( Resources (..),
  )
import Linear
import SDL
  ( Point (P),
    Rectangle (..),
  )
import Game1.Map

intersectsWith :: Rectangle CInt -> Rectangle CInt -> Bool
intersectsWith
  (Rectangle (P (V2 px py)) (V2 pw ph))
  (Rectangle (P (V2 ox oy)) (V2 ow oh)) =
    px < ox + ow
      && px + pw > ox
      && py < oy + oh
      && py + ph > oy

updatePlayer :: Map -> V2 Int -> Player -> Player
updatePlayer m delta p@(Player v@(V2 v1 v2) speed) =
  p & playerPos .~ newPos
  where
    newPos =
      let u = v + fmap (* speed) delta
          tt = getTileType u m
       in case tt of
            Empty -> u
            _ -> v

renderPlayer :: (MonadIO m, MonadReader Resources m) => Player -> m ()
renderPlayer (Player v _) = do
  (tx, _) <- asks tex_player
  let target = fmap (CInt . (32 *) . fromIntegral) v
  renderTexture tx target
