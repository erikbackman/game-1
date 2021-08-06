{-# LANGUAGE TemplateHaskell #-}

module GameState
  (GameState(..), playerPos)
where

import SDL
import Foreign.C
import Control.Lens

data GameState = GameState
  { _playerPos :: Point V2 CInt
  }

makeLenses ''GameState
