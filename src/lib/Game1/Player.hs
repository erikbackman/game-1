module Game1.Player where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
  ( MonadReader,
    asks,
  )
import Foreign.C (CInt)
import Game1.Render (renderTexture)
import Game1.Resources
  ( Resources (..),
    sdl_renderer,
    tex_player,
  )
import Game1.Window (withinBounds)
import SDL
  ( Point (P),
    Rectangle (..),
    V2 (V2),
  )

intersectsWith :: Rectangle CInt -> Rectangle CInt -> Bool
intersectsWith
  (Rectangle (P (V2 px py)) (V2 pw ph))
  (Rectangle (P (V2 ox oy)) (V2 ow oh)) =
    px < ox + ow
      && px + pw > ox
      && py < oy + oh
      && py + ph > oy

nextPlayerPos :: V2 CInt -> Rectangle CInt -> Rectangle CInt
nextPlayerPos delta pr@(Rectangle pp@(P (V2 px py)) wh@(V2 pw ph)) =
  let newPos = pp + P delta
      np = Rectangle newPos wh
   in if withinBounds newPos then np else pr

renderPlayer :: (MonadIO m, MonadReader Resources m) => Rectangle CInt -> m ()
renderPlayer (Rectangle pos _) = do
  renderer <- asks sdl_renderer
  (tx, _) <- asks tex_player
  renderTexture renderer tx pos

-- TODO Move/fix this shit
renderEnemy :: (MonadIO m, MonadReader Resources m) => Rectangle CInt -> m ()
renderEnemy (Rectangle pos _) = do
  renderer <- asks sdl_renderer
  (tx, _) <- asks tex_enemy
  renderTexture renderer tx pos
