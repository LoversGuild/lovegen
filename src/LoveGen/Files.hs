{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : LoveGen.Files
-- Description : File and file system operations for LoveGen
-- Copyright   : Copyright (C) 2023-2024 The Lovers' Guild
-- License     : AGPL-3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides file and file system operations for LoveGen.
module LoveGen.Files (
    -- * File management
    readTextFile,
    writeTextFile,
    readBinaryFile,
    writeBinaryFile,
    listDirectoryRecursive,
    listDirectoryAsTrie,
)
where

import Control.Monad (foldM, when)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.Encoding
import GHC.Stack
import System.Directory.OsPath
import System.File.OsPath qualified as OP
import System.OsPath

import LoveGen.RoseTrie

-- | Like if, but condition can be monadic.
-- Thsi function is only needed inside this module and stays here until the sitaution changes.
ifM
    :: Monad m
    => m Bool
    -- ^ Conditional
    -> m a
    -- ^ Then action
    -> m a
    -- ^ Else action
    -> m a
ifM cond true false = cond >>= bool false true

-- | Read a text file (in UTF-8).
--
-- Throws an exception on invalid UTF-8 input
readTextFile :: HasCallStack => OsPath -> IO T.Text
readTextFile = fmap decodeUtf8 . readBinaryFile

-- | Write a text file with UTF-8 encoding
writeTextFile :: HasCallStack => OsPath -> T.Text -> IO ()
writeTextFile fp text = writeBinaryFile fp $! encodeUtf8 text

-- | Read a file of bytes
readBinaryFile :: HasCallStack => OsPath -> IO BS.ByteString
readBinaryFile fp = OP.readFile' fp

-- | Write a file of bytes
writeBinaryFile :: HasCallStack => OsPath -> BS.ByteString -> IO ()
writeBinaryFile fp bytes = do
    createDirectoryIfMissing True $! takeDirectory fp
    doesFileExist fp >>= flip when (removeFile fp)
    OP.writeFile' fp bytes

-- | Recursively list all paths under a subdirectory.
listDirectoryRecursive :: OsPath -> IO [OsPath]
listDirectoryRecursive dir = fmap reverse $! scanDir [dir] dir
  where
    -- Scan a subdirectory and return all paths found so far
    scanDir :: [OsPath] -> OsPath -> IO [OsPath]
    scanDir found fp =
        listDirectory fp
            >>= foldM scanEntry found . fmap (fp </>)

    -- Scan a single directory entry and return a list of all paths found so far
    scanEntry :: [OsPath] -> OsPath -> IO [OsPath]
    scanEntry found path =
        let found' = path : found
        in  doesDirectoryExist path
                >>= bool (pure $! found') (scanDir found' path)

-- | List a directory recursively and return all entries represented as a
-- RoseTrie. The path to each node is built off the directory names leading to
-- the node, and the node itself is the full path of the node.
listDirectoryAsTrie
    :: OsPath
    -- ^ Root of the directory to scan
    -> IO (RoseTrie OsPath OsPath)
listDirectoryAsTrie rootDir =
    let prefixLength = length $! splitDirectories rootDir
    in  ( listDirectoryRecursive rootDir
            <&> (roseTrieFromList . fmap (\p -> (drop prefixLength $! splitDirectories p, p)))
        )
