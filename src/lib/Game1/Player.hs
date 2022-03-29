module Game1.Player where

import Control.Monad.RWS
import Foreign.C (CInt)
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

isTileEmpty :: TileType -> Bool
isTileEmpty Empty = True
isTileEmpty _ = False

orentation :: V2 Int -> V2 Bool
orentation (V2 v1 v2) = V2 ((< 0) v1) False

move :: Map -> V2 Int -> Player -> Player
move m dirv@(V2 d1 d2) p@(Player posv@(V2 p1 p2) speed curr_dirv _) =
  let new_dirv = if d1 /= 0 then dirv else curr_dirv
      new_posv = posv + speed *^ dirv
      can_move = isTileEmpty (getTileType new_posv m)
   in if can_move then p {_playerPos = new_posv, _playerDir = new_dirv} else p

renderPlayer :: (MonadIO m, MonadReader Resources m) => Player -> m ()
renderPlayer (Player v@(V2 v1 v2) _ d state) = do
  (tx, _) <- asks tex_player
  renderTexture tx v (orentation d)
