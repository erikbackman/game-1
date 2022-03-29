module Game1.Scene where

import Control.Lens (use)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
  ( MonadReader,
  )
import Control.Monad.State.Class
  ( MonadState,
  )
import Game1.GameState (GameState (..), gs_map, gs_player, drawMap)
import Game1.Player (renderPlayer)
import Game1.Resources (Resources (..))
import Game1.Render (renderTexture)
import Foreign.C
import SDL.Vect
import Control.Monad.RWS

renderHeart :: (MonadIO m, MonadReader Resources m) => m ()
renderHeart = do
  (tex, _) <- asks tex_heart
  let target = fmap (CInt . (32 *) . fromIntegral) (V2 10 0) 
  renderTexture tex target (V2 False False)
  
drawScene ::
  (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
drawScene = do
  p <- use gs_player
  m <- use gs_map

  drawMap m
  renderPlayer p
  renderHeart
