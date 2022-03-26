{-# LANGUAGE BlockArguments #-}

module Game1.Input where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Semigroup
  ( Any (Any),
    Sum (Sum),
  )
import SDL

data Intent
  = Quit
  | Idle
  | Move (V2 Int)

e1 = V2 1 0
e2 = V2 0 1

pollEventPayloads :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads = liftIO $ fmap SDL.eventPayload <$> SDL.pollEvents

inputToIntent :: [SDL.EventPayload] -> Intent
inputToIntent evps =
  let (Any quit, Sum posDelta) = flip
        foldMap
        evps
        \case
          SDL.QuitEvent -> (Any True, mempty)
          SDL.KeyboardEvent e ->
            if SDL.keyboardEventKeyMotion e == SDL.Pressed
              then case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                SDL.ScancodeQ -> (Any True, mempty)
                SDL.ScancodeW -> (Any False, Sum $ -e2)
                SDL.ScancodeS -> (Any False, Sum    e2)
                SDL.ScancodeA -> (Any False, Sum $ -e1)
                SDL.ScancodeD -> (Any False, Sum    e1)
                _ -> mempty
              else mempty
          _ -> mempty
   in if quit then Quit else Move posDelta
