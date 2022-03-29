module Game1.Player where

import Control.Monad.RWS
import Game1.GameState
import Game1.Map
import Game1.Render (renderTexture)
import Game1.Resources
  ( Resources (..),
  )
import Linear

isTileEmpty :: TileType -> Bool
isTileEmpty Empty = True
isTileEmpty _ = False

orentation :: V2 Int -> V2 Bool
orentation (V2 v1 v2) = V2 ((< 0) v1) False

move :: Map -> V2 Int -> Player -> Player
move m dirv@(V2 d1 d2) p@(Player posv@(V2 p1 p2) speed curr_dirv _) =
  let -- Only care to update dirv when d1 != 0 since
      -- I don't have any sprites for back and front atm.
      new_dirv = if d1 /= 0 then dirv else curr_dirv
      --
      new_posv = posv + speed *^ dirv
      can_move = isTileEmpty (getTileType new_posv m)
   in if can_move then p {_playerPos = new_posv, _playerDir = new_dirv} else p

renderPlayer :: (MonadIO m, MonadReader Resources m) => Player -> m ()
renderPlayer (Player v@(V2 v1 v2) _ d state) = do
  (tx, _) <- asks tex_player
  renderTexture tx v (orentation d)
