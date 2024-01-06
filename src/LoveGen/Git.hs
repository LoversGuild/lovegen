-- |
-- Module      : LoveGen.Git
-- Description : Git commit time retrieval functions
-- Copyright   : Copyright (C) 2023-2024 The Lovers' Guild
-- License     : AGPL-3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides functions for retrieving files' commit times from a
-- git repository.
module LoveGen.Git (
    fetchFirstCommitTime,
    fetchLastCommitTime,
)
where

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time
import Data.Time.Format.ISO8601
import System.OsPath
import System.Process.Typed

-- | Fetch a commit datetime of a file from git repository
fetchGitCommitTime
    :: [String]
    -- ^ Additional command-line options to git
    -> OsPath
    -- ^ Path to the file to be queried
    -> IO (Maybe UTCTime)
fetchGitCommitTime gitOpts fp = do
    stringFP <- decodeFS fp
    (code, out) <-
        readProcessStdout $!
            setEnv [("TZ", "UTC")] $!
                proc "git" (["log", "-1", "--format=%aI"] <> gitOpts <> ["--", stringFP])
    if code /= ExitSuccess
        then pure Nothing
        else pure $! iso8601ParseM . T.unpack . decodeUtf8 . BL.toStrict $! out

-- | Fetch the date and time of the first commit of a file
fetchFirstCommitTime :: OsPath -> IO (Maybe UTCTime)
fetchFirstCommitTime = fetchGitCommitTime ["--diff-filter=A", "--follow", "--find-renames=40%"]

-- | Fetch the date and time of the latest commit of a file
fetchLastCommitTime :: OsPath -> IO (Maybe UTCTime)
fetchLastCommitTime = fetchGitCommitTime []
