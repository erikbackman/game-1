module Game1.Player where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
  ( MonadReader,
    asks,
  )
import Foreign.C (CInt)
import Game1.Render (renderTexture)
import Game1.Resources
  ( Resources (..),
    sdl_renderer,
    tex_player,
  )
import Game1.Window (withinBounds)
import SDL
  ( Point (P),
    V2 (V2),
  )

startPosition :: Point V2 CInt
startPosition = P $ V2 100 100

nextPlayerPos :: V2 CInt -> Point V2 CInt -> Point V2 CInt
nextPlayerPos delta current =
  let newPos = current + P delta
   in if withinBounds newPos then newPos else current

renderPlayer :: (MonadIO m, MonadReader Resources m) => Point V2 CInt -> m ()
renderPlayer pos = do
  renderer <- asks sdl_renderer
  image <- asks tex_player
  renderTexture renderer image pos
