module Game1.Scene where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
  ( MonadReader,
    asks,
  )
import Control.Monad.State.Class
  ( MonadState,
    gets,
  )
import Game1.GameState (GameState (..))
import Game1.Player (renderEnemy, renderPlayer)
import Game1.Resources (Resources (..))

drawScene ::
  (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
drawScene = do
  r <- asks sdl_renderer
  p <- gets _playerRect
  e <- gets _enemyRect
  renderPlayer p
  renderEnemy e
