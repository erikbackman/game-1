module Game1.Player where

import           Foreign.C    (CInt)
import           Game1.Window (withinBounds)
import           SDL          (Point (P), Renderer, V2 (V2), _x, _y, ($=))

startPosition :: Point V2 CInt
startPosition = P $ V2 100 100

nextPlayerPos :: V2 CInt -> Point V2 CInt -> Point V2 CInt
nextPlayerPos delta current = let newPos = current + P delta
                              in if withinBounds newPos then newPos else current
