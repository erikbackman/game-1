module Game1.Scene where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
  ( MonadReader,
  )
import Control.Monad.State.Class
  ( MonadState,
    gets,
  )
import Game1.GameState (GameState (..), drawMap)
import Game1.Player (renderPlayer)
import Game1.Resources (Resources (..))

drawScene ::
  (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
drawScene = do
  p <- gets _gsPlayer
  m <- gets _gsMap

  drawMap m
  renderPlayer p
