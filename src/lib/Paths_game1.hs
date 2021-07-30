module Paths_game1 where

import Control.Monad.Trans (MonadIO)

getDataFileName :: MonadIO m => FilePath -> m FilePath
getDataFileName = pure
