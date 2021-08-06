{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Game1 where

import           Control.Concurrent     (threadDelay)
import           Control.Lens           ((+=))
import           Control.Monad          (void)
import           Control.Monad.Extra    (whileM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader (..),
                                         ReaderT (ReaderT, runReaderT), asks,
                                         reader, runReader)
import           Control.Monad.State    (MonadIO (..), MonadState, StateT,
                                         evalStateT, gets)
import           Data.Foldable          (foldl')
import           Data.Monoid            (Any (Any))
import           Data.Semigroup         (Sum (Sum))
import           Data.Text              (Text)
import           Foreign.C              (CInt)
import           Paths_game1            ()
import           SDL                    (Point (P), Renderer, V2 (V2), ($=))

import           Game1.GameState        (GameState (..), playerPos)
import           Game1.Resources        (Resources (..), loadResources)

import qualified SDL
import qualified SDL.Image

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow
    "Game1"
    SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }

  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  assets   <- loadResources renderer

  let
    env =
      Env { env_assets = assets, env_renderer = renderer, env_window = window }
    initState =
      GameState { _playerPos = startPosition }

  runGame1 env initState mainLoop
  cleanupAndQuitSDL env

cleanupAndQuitSDL :: Env -> IO ()
cleanupAndQuitSDL env = do
  SDL.destroyTexture $ tex_box $ env_assets env
  SDL.destroyRenderer $ env_renderer env
  SDL.destroyWindow $ env_window env
  SDL.quit

startPosition :: Point V2 CInt
startPosition = P $ V2 100 100

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

newtype Game1 a =
  Game1 (ReaderT Env (StateT GameState IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState GameState, MonadIO)

data Env = Env
  { env_assets   :: Resources
  , env_renderer :: SDL.Renderer
  , env_window   :: SDL.Window
  }

data Intent
  = Quit
  | Idle
  | Move (V2 CInt)

runGame1 :: Env -> GameState -> Game1 a -> IO a
runGame1 r s (Game1 m) = evalStateT (runReaderT m r) s


renderTexture
  :: MonadIO m => SDL.Renderer -> SDL.Texture -> (Point V2 CInt) -> m ()
renderTexture renderer tex pos = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      extent = V2 w h
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle pos extent)

pollEventPayloads :: MonadIO m => m [SDL.EventPayload]
pollEventPayloads = liftIO $ fmap SDL.eventPayload <$> SDL.pollEvents

renderPlayer :: (MonadIO m, MonadReader Env m) => Point V2 CInt -> m ()
renderPlayer pos = do
  renderer <- asks env_renderer
  image    <- asks (tex_box . env_assets)
  renderTexture renderer image pos

mainLoop :: (MonadIO m, MonadReader Env m, MonadState GameState m) => m ()
mainLoop = do
  renderer <- asks env_renderer
  loop renderer
 where
  loop r = do
    input <- pollEventPayloads
    case inputToIntent input of
      Quit       -> pure ()
      Idle       -> step r
      Move delta -> playerPos += P delta >> step r

  step r' = do
    p <- gets _playerPos
    SDL.clear r'
    renderPlayer p
    SDL.present r'
    liftIO $ threadDelay (1000 * 16)
    loop r'

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
    if quit then Quit else Move $ posDelta

