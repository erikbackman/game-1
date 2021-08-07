{-# LANGUAGE TemplateHaskell #-}

module Game1.GameState (GameState (..), playerPos) where

import Control.Lens (makeLenses)
import Foreign.C (CInt)
import SDL (Point, V2)

data GameState = GameState
  { _playerPos :: Point V2 CInt
  }

makeLenses ''GameState
