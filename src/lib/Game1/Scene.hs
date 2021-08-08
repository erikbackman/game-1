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
import Foreign.C (CInt)
import Game1.GameState (GameState (..))
import Game1.Player (renderPlayer)
import Game1.Resources (Resources (..))
import Linear (V2 (V2))
import SDL (Point (P), Rectangle (Rectangle))

drawScene ::
  (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
drawScene = do
  r <- asks sdl_renderer
  p <- gets _playerRect
  let Rectangle _ size = p :: Rectangle CInt
      p2Pos = Rectangle (P (V2 500 500)) size :: Rectangle CInt
  renderPlayer p
  renderPlayer p2Pos
