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
  renderTexture tex (V2 10 0) (V2 False False)
  
drawScene ::
  (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
drawScene = do
  p <- use gs_player
  m <- use gs_map

  drawMap m
  renderPlayer p
  renderHeart
