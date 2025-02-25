{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : LoveGen.Pandoc
-- Description : Utility functions for working with Pandoc
-- Copyright   : Copyright (C) 2022–2025 The Lovers' Guild
-- License     : GNU Affero General Public License version 3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides high-level helpers for working with Pandoc.
module LoveGen.Pandoc (
    -- * High-level interface
    loadTemplate,
    loadYamlMeta,
    readMarkdownFile,

    -- * Pandoc format conversion
    PandocReader,
    PandocWriter,
    readPandoc,
    renderPandoc,
    runPandoc,

    -- * Metadata management
    verifyMetaKeys,
    getDocMeta,
    setDocMeta,
    modifyMeta,
    metaUnion,
    addMeta,
    lookupMetaForce,
)
where

import Control.Monad (when, (>=>))
import Data.HashSet qualified as HS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import System.OsPath
import Text.Pandoc
import Text.Pandoc.Readers.Markdown (yamlToMeta)

import LoveGen.Files

-- | Load a template from file
loadTemplate :: HasCallStack => OsPath -> IO (Template T.Text)
loadTemplate fp = do
    stringFP <- decodeFS fp
    text <- readTextFile fp
    compileTemplate stringFP text
        >>= either fail pure

-- | Load metadata from YAML file
loadYamlMeta :: ReaderOptions -> OsPath -> IO Meta
loadYamlMeta opts fp = do
    stringFP <- decodeFS fp
    readBinaryFile fp >>= runPandoc . yamlToMeta opts (Just stringFP)

-- | Read a markdown file into a pandoc document
readMarkdownFile :: ReaderOptions -> OsPath -> IO Pandoc
readMarkdownFile opts fp =
    readTextFile fp >>= readPandoc (readMarkdown opts) fp

-- | A type for a Pandoc reader function. Make one by applying a Pandoc reader to ReaderOptions.
type PandocReader = [(FilePath, T.Text)] -> PandocIO Pandoc

-- | A type for a Pandoc writer function. Make one by applying a Pandoc writer to WriterOptions.
type PandocWriter = Pandoc -> PandocIO T.Text

-- | Read a document with a specified reader function
readPandoc
    :: PandocReader
    -- ^ Reader to use for converting the text to AST
    -> OsPath
    -- ^ Name of the file (for error messages)
    -> T.Text
    -- ^ Text to convert
    -> IO Pandoc
readPandoc reader fp content = do
    stringFP <- decodeFS fp
    runPandoc $! reader [(stringFP, content)]

-- | Render a Pandoc document with a writer function
renderPandoc
    :: PandocWriter
    -> Pandoc
    -> IO T.Text
renderPandoc writer doc =
    runPandoc $! writer doc

-- | Run Pandoc inside IO
runPandoc :: PandocIO a -> IO a
runPandoc =
    runIO >=> either (fail . show) pure

-- metaToJson :: PandocWriter -> Meta -> IO A.Value
-- metaToJson writer (Meta meta)
--     = runPandoc $! A.toJSON <$> traverse go meta
--   where
--     go :: MetaValue -> PandocIO A.Value
--     go (MetaMap m) = A.toJSON <$> traverse go m
--     go (MetaList m) = A.toJSONList <$> traverse go m
--     go (MetaBool m) = pure $! A.toJSON m
--     go (MetaString m) = pure $! A.toJSON m
--     go (MetaInlines m) = A.toJSON <$> (writer . Pandoc mempty . (: []) . Plain $! m)
--     go (MetaBlocks m) = A.toJSON <$> (writer . Pandoc mempty $! m)

-- | Verify that a JSON metadata object has all desired keys, and all of them
verifyMetaKeys
    :: HasCallStack
    => HS.HashSet T.Text
    -- ^ A set of mndatory keys
    -> HS.HashSet T.Text
    -- ^ A set of optional keys
    -> Meta
    -- ^ The metadata object
    -> IO ()
verifyMetaKeys required optional (Meta mmap) =
    let present = HS.fromList $! M.keys mmap
        desired = HS.union optional required
        missing = HS.difference required present
        surplus = HS.difference present desired
    in  ( when (not $! HS.null missing && HS.null surplus) $
            error $!
                "Metadata verification failed -"
                    <> " missing keys: "
                    <> show missing
                    <> ", excess keys: "
                    <> show surplus
        )

-- | Extract metadata from a Pandoc document
getDocMeta :: Pandoc -> Meta
getDocMeta (Pandoc meta _) = meta

-- | Replace old metadata in a document entirely.
-- | Modify the metadata embedded in a Pandoc
setDocMeta :: Meta -> Pandoc -> Pandoc
setDocMeta meta (Pandoc _ blocks) =
    Pandoc meta blocks

-- | Modify the metadata embedded in a Pandoc
modifyMeta :: (Meta -> Meta) -> Pandoc -> Pandoc
modifyMeta f (Pandoc meta blocks) =
    Pandoc (f meta) blocks

-- | Combine Meta values. Maps are joined together such that keys in the first
-- map take precedence. Lists are concatenated. Every other value is
-- overwritten with the one in the first Meta. If values are not of compatible
-- type, the first value takes precedence.
metaUnion :: Meta -> Meta -> Meta
metaUnion (Meta ma) (Meta mb) = Meta $! M.unionWith combineMeta ma mb
  where
    combineMeta :: MetaValue -> MetaValue -> MetaValue
    combineMeta (MetaMap a) (MetaMap b) = MetaMap $! M.unionWith combineMeta a b
    combineMeta (MetaList a) (MetaList b) = MetaList $! a <> b
    combineMeta a _ = a

-- | Add a key/value pair to metadata
addMeta :: T.Text -> MetaValue -> Meta -> Meta
addMeta key val (Meta mmap) = Meta $! M.insert key val mmap

-- | Forcefully lookup a meta value
lookupMetaForce :: HasCallStack => T.Text -> Meta -> MetaValue
lookupMetaForce key meta =
    case lookupMeta key meta of
        Just val -> val
        Nothing ->
            error $ "Required metadata key " <> show key <> " was not found from metadata: " <> show meta
