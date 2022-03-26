module Game1.Scene where

import Control.Lens (use)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
  ( MonadReader,
  )
import Control.Monad.State.Class
  ( MonadState,
  )
import Game1.GameState (GameState (..), gs_map, gs_player)
import Game1.Map
import Game1.Player (renderPlayer)
import Game1.Resources (Resources (..))

drawScene ::
  (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
drawScene = do
  p <- use gs_player
  m <- use gs_map

  drawMap m
  renderPlayer p
