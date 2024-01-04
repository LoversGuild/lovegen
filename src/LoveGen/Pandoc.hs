{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : LoveGen.Pandoc
-- Description : Utility functions for working with Pandoc
-- Copyright   : Copyright (C) 2023-2024 The Lovers' Guild
-- License     : AGPL-3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides high-level helpers for working with Pandoc.
module LoveGen.Pandoc (
    -- * Pandoc format conversion
    PandocReader,
    PandocWriter,
    readPandoc,
    renderPandoc,
    runPandoc,

    -- * Metadata management
    verifyMetaKeys,
    getDocMeta,
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
