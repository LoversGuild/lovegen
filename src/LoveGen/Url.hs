-- |
-- Module      : LoveGen.Url
-- Description : Url handling
-- Copyright   : Copyright (C) 2023-2024 The Lovers' Guild
-- License     : AGPL-3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- Thsi module provides utilities and data type for handling URLs.
module LoveGen.Url (
    Url,
    osPathToUrl,
    urlToOsPath,
)
where

import Control.Monad ((>=>))
import Data.Text qualified as T
import System.OsPath

-- | Type alais for storing URLs as Text
type Url = T.Text

-- | Convert an OsPath to Url
osPathToUrl :: OsPath -> IO Url
osPathToUrl = decodeFS >=> pure . T.pack

-- | Convert an Url to OsPath
urlToOsPath :: Url -> IO OsPath
urlToOsPath url
    | T.null url = pure $! [osp|.|]
    | otherwise = encodeFS . T.unpack $! url
