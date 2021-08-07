module Game1.Scene where

import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader.Class (MonadReader, asks)
import           Control.Monad.State.Class  (MonadState, gets)
import           SDL                        (Point)
import           SDL.Vect                   (V2 (V2), Point (P))
import           Foreign.C                  (CInt (CInt))

import           Game1.GameState            (GameState (..))
import           Game1.Player
import           Game1.Resources            (Resources (..))
import Game1.Render (renderTexture)

drawScene :: (MonadIO m, MonadReader Resources m, MonadState GameState m) => m ()
drawScene = do
  r <- asks sdl_renderer
  p <- gets _playerPos
  renderPlayer p
