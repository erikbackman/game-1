{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Game1.GameState (GameState (..), playerRect, initGameState) where

import Control.Lens (makeLenses)
import Foreign.C (CInt)
import Game1.Resources (Resources (..))
import Linear.V2 (V2 (V2))
import SDL (Point, Rectangle)
import qualified SDL
import SDL.Vect (Point (P))
import SDL.Video (Rectangle (Rectangle))

data GameState = GameState
  { _playerRect :: SDL.Rectangle CInt,
    _enemyRect :: SDL.Rectangle CInt
  }
  deriving stock (Show)

startPosition :: Point V2 CInt
startPosition = P $ V2 100 100

enemeyStartPosition :: Point V2 CInt
enemeyStartPosition = P $ V2 300 300

initGameState :: Resources -> GameState
initGameState Resources {tex_player} =
  let ti = snd tex_player
      (pw, ph) = (SDL.textureWidth ti, SDL.textureHeight ti)
      pr = Rectangle startPosition (V2 pw ph)
      er = Rectangle enemeyStartPosition (V2 pw ph)
   in GameState {_playerRect = pr, _enemyRect = er}

makeLenses ''GameState
