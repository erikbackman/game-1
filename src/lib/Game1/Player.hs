module Game1.Player where

import Control.Lens ((^.))
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
import Game1.Map
import Game1.Render (renderTexture)
import Game1.Resources
  ( Resources (..),
  )
import Linear
import SDL
  ( Point (P),
    Rectangle (..),
  )

intersectsWith :: Rectangle CInt -> Rectangle CInt -> Bool
intersectsWith
  (Rectangle (P (V2 px py)) (V2 pw ph))
  (Rectangle (P (V2 ox oy)) (V2 ow oh)) =
    px < ox + ow
      && px + pw > ox
      && py < oy + oh
      && py + ph > oy

updatePlayer :: Map -> V2 Int -> Player -> Player
updatePlayer m dir_vec p@(Player v@(V2 v1 v2) speed d) =
  p & playerPos
    .~ newPos & playerDir
    .~ (if dir_vec ^. _x == 0 then d else dir_vec)
    
  where
    newPos =
      let u = v + fmap (* speed) dir_vec
          tt = getTileType u m
       in case tt of
            Empty -> u
            _ -> v

renderPlayer2 :: (MonadIO m, MonadReader Resources m) => Player -> m ()
renderPlayer2 = undefined

renderPlayer :: (MonadIO m, MonadReader Resources m) => Player -> m ()
renderPlayer (Player v _ d) = do
  (tx, _) <- asks tex_player
  let target = fmap (CInt . (32 *) . fromIntegral) v
      tex_dir = V2 (d ^. _x == (-1)) False
  renderTexture tx target tex_dir
