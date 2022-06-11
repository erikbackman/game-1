module Game1.Scene where

import Control.Lens (use)
import Game1.GameState (GameState (..), gs_map, gs_player, drawMap)
import Game1.Player (renderPlayer)
import Game1.Resources (Resources (..))
import Game1.Render (renderTexture)
import SDL.Vect
import Control.Monad.RWS

renderHeart :: (MonadIO m, MonadReader Resources m) => m ()
renderHeart = do
  (tex, _) <- asks tex_heart
  renderTexture (V2 10 0) tex

drawScene ::
  (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
drawScene = do
  p <- use gs_player
  m <- use gs_map
  
  drawMap m
  renderPlayer p
  renderHeart
