module Game1.Player where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
  ( MonadReader,
    asks,
  )
import Foreign.C (CInt)
import Game1.Render (renderTexture)
import Game1.Resources
  ( Resources (..) )
import SDL
  ( Point (P),
    Rectangle (..),
  )
import Game1.GameState
import Foreign.C.Types (CInt(CInt))
import Linear

intersectsWith :: Rectangle CInt -> Rectangle CInt -> Bool
intersectsWith
  (Rectangle (P (V2 px py)) (V2 pw ph))
  (Rectangle (P (V2 ox oy)) (V2 ow oh)) =
    px < ox + ow
      && px + pw > ox
      && py < oy + oh
      && py + ph > oy

nextPlayerPos :: Map -> V2 Int -> V2 Int -> V2 Int
nextPlayerPos m delta v@(V2 v1 v2) =
  let 
      u  = v + delta
      tt = getTileType u m
   in
    case tt of
      Empty -> u
      _     -> v

renderPlayer :: (MonadIO m, MonadReader Resources m) => Player -> m ()
renderPlayer (Player v speed) = do
  (tx, _) <- asks tex_player
  let
    u = fmap (CInt . (speed*32*) . fromIntegral) v
  renderTexture tx u
