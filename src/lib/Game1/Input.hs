{-# LANGUAGE BlockArguments #-}
module Game1.Input where

import qualified SDL
import Data.Semigroup (Any(Any), Sum (Sum))
import SDL
import Foreign.C
import Control.Monad.IO.Class (MonadIO (liftIO))

data Intent
  = Quit
  | Idle
  | Move (V2 CInt)

pollEventPayloads :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads = liftIO $ fmap SDL.eventPayload <$> SDL.pollEvents

inputToIntent :: [SDL.EventPayload] -> Intent
inputToIntent evps =
  let
    (Any quit, Sum posDelta) =
      flip foldMap
        evps
        \case
          SDL.QuitEvent -> (Any True, mempty)

          SDL.KeyboardEvent e ->
            if
            | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
                  SDL.ScancodeQ -> (Any True, mempty)
                  SDL.ScancodeW -> (Any False, Sum (V2 0 (-10)))
                  SDL.ScancodeS -> (Any False, Sum (V2 0 10))
                  SDL.ScancodeA -> (Any False, Sum (V2 (-10) 0))
                  SDL.ScancodeD -> (Any False, Sum (V2 10 0))
                  _             -> mempty
            | otherwise -> mempty

          otherwise -> mempty
  in
    if quit then Quit else Move posDelta
