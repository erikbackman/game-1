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
    V2 (V2),
  )
import Game1.GameState
import Foreign.C.Types (CInt(CInt))

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
      tv = getTileType u m
   in
    case tv of
      Empty -> u
      _     -> v

renderPlayer :: (MonadIO m, MonadReader Resources m) => Player -> m ()
renderPlayer (Player (V2 v1 v2) speed) = do
  (tx, _) <- asks tex_player
  let u1 = CInt $ speed*32*fromIntegral v1 :: CInt
      u2 = CInt $ speed*32*fromIntegral v2 :: CInt
      u  = V2 u1 u2
  renderTexture tx u
