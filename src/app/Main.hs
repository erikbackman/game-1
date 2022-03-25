module Main where

import qualified Game1
import Paths_game1
import Control.Monad
import Control.Applicative (liftA2)
import Data.Foldable
import Data.List
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

main :: IO ()
main = Game1.main 
