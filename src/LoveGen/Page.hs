-- |
-- Module      : LoveGen.Page
-- Description : High-level page representation and processing
-- Copyright   : Copyright (C) 2022-2024 The Lovers' Guild
-- License     : GNU Affero General Public License version 3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines a data structure for representing pages along with
-- associated high-level functions for page management (loading and
-- rendering). Most functions in this module require a 'Config' instance to
-- control their behavior.
module LoveGen.Page (
    Page (..),
    loadPage,
    loadPagesDir,
    writePage,
) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Bool
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Stack (HasCallStack)
import System.Directory.OsPath
import System.OsPath
import Text.Pandoc hiding (getModificationTime)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Transforms (headerShift)
import Text.Pandoc.Writers.Shared (lookupMetaString)

import LoveGen.Config
import LoveGen.Files
import LoveGen.Git
import LoveGen.Pandoc
import LoveGen.RoseTrie
import LoveGen.Url

-- | Representation of a single page
data Page = Page
    { -- | URL fragment
      url :: Url,
      -- | URL split into path segments
      urlSegments :: [Url],
      -- | Absolute URL of the page
      absoluteUrl :: Url,
      -- | Contents and metadata
      doc :: Pandoc,
      -- | Title of the page in a menu
      menuTitle :: MetaValue,
      -- | Sorting score for the page in a menu
      order :: Int,
      -- | Language the page is written in
      lang :: T.Text,
      -- | Is this page hidden from menus
      hidden :: Bool,
      -- | Template that is to be used to render this page
      template :: Template T.Text,
      -- | Name of the destination file
      destFile :: OsPath
    }
    deriving stock (Eq, Show)

-- | Load a single page from a Markdown file.
loadPage
    :: HasCallStack
    => SiteConfig
    -- ^ Website configuration
    -> Meta
    -- ^ Metadata inherited from meta.yaml files or other sources
    -> OsPath
    -- ^ FIle name of the page to be loaded
    -> IO Page
loadPage config baseMeta fp = do
    putStrLn $ "Loading page " <> show fp
    doc <- readMarkdownFile config.markdownReaderOptions fp <&> (headerShift config.shiftHeadings)

    -- Metadata handling
    let meta = metaUnion (getDocMeta doc) baseMeta
    let pagesDirSegments = splitDirectories $! config.pagesDir
        pagesDirSegmentsCount = length pagesDirSegments

    -- Calculate relative url and base target file name either based on the
    -- page file name or the "url" metadata field. Avoid extraneous
    -- encoding/decoding of the file name.
    (url, baseDestFile) <- case lookupMeta "url" $! meta of
        Just metaUrl -> do
            let urlTrimmed = T.dropAround (== '/') . stringify $! metaUrl
                urlSlashed = if T.null urlTrimmed then "" else urlTrimmed <> "/"
            dest' <- urlToOsPath urlSlashed
            pure (urlSlashed, dest')
        Nothing -> do
            let dest' = joinPath . drop pagesDirSegmentsCount . splitDirectories . dropExtension $! fp
            url' <- osPathToUrl dest'
            pure (url' <> "/", dest')

    let destFile = normalise $! config.outputDir </> baseDestFile </> config.indexFileName
        urlSegments = filter (not . T.null) . T.split (== '/') $! url
        absoluteUrl = config.siteUrl <> url
        rootUrlRelative =
            if null urlSegments
                then "./"
                else T.replicate (length urlSegments) "../"

    time <-
        fetchLastCommitTime fp >>= \case
            Just t -> pure $! t
            Nothing -> getModificationTime fp
    creationTime <- fetchFirstCommitTime fp <&> fromMaybe time

    let lang = lookupMetaString "lang" meta
    let locale =
            fromMaybe
                (error $ "No locale settings for language `" <> show lang <> "` were found.")
                $! HM.lookup lang config.locales

    let meta' =
            addMeta "site-root" (MetaString rootUrlRelative)
                . addMeta "site-url" (MetaString config.siteUrl)
                . addMeta "absolute-url" (MetaString absoluteUrl)
                . addMeta "url" (MetaString if T.null url then "./" else url)
                . addMeta "date-meta" (MetaString . T.pack $! iso8601Show time)
                . addMeta "date" (MetaString . T.pack $! formatTime locale.timeLocale (T.unpack locale.dateFormat) time)
                . addMeta "creation-date-meta" (MetaString . T.pack $! iso8601Show creationTime)
                . addMeta
                    "creation-date"
                    (MetaString . T.pack $! formatTime locale.timeLocale (T.unpack locale.dateFormat) creationTime)
                $! meta
        doc' = setDocMeta meta' doc

    verifyMetaKeys config.requiredMetadata config.optionalMetadata meta'

    let menuTitle =
            fromMaybe (error "Impossible: page has no title") $
                lookupMeta "menu-title" meta'
                    <|> lookupMeta "title" meta'
        order = either error fst . T.decimal . stringify . lookupMetaForce "order" $ meta'
        hidden = case lookupMeta "hidden" meta' of
            Just (MetaBool val) -> val
            Nothing -> False
            val -> error $ "Invalid value for meta key 'hidden': " <> show val

    templateFile <- encodeFS . T.unpack . stringify . lookupMetaForce "template" $ meta'
    template <- loadTemplate $! config.templatesDir </> templateFile

    pure $!
        Page
            { url = url,
              urlSegments = urlSegments,
              absoluteUrl = absoluteUrl,
              doc = doc',
              menuTitle = menuTitle,
              lang = lang,
              order = order,
              hidden = hidden,
              template = template,
              destFile = destFile
            }

-- | Recursively load all pages in a directory. This function currently only
-- supports pages in Markdown format and with .md extension. Before loading
-- pages of each directory, a meta.yaml file is loaded, if it exists and the
-- metadata from this file is used to provide default values for all files in
-- this directory (and its subdirectories).
loadPagesDir :: HasCallStack => SiteConfig -> IO [Page]
loadPagesDir config = do
    -- Build a tree from the directory hierarchy.
    pathTrie <- listDirectoryAsTrie config.pagesDir
    loadPagesRecursive nullMeta pathTrie
  where
    loadPagesRecursive :: Meta -> RoseTrie OsPath OsPath -> IO [Page]
    loadPagesRecursive meta (TrieNode path subforest)
        | HM.null subforest = do
            -- This might be a file
            isFile <- doesFileExist path
            if isFile && isExtensionOf [osp|md|] path
                then fmap (: []) $! loadPage config meta path
                else pure []
        | otherwise = do
            -- This is a directory
            let metaYamlFile = path </> config.metaYamlFile
            meta' <-
                doesFileExist metaYamlFile
                    >>= bool (pure nullMeta) (loadYamlMeta config.markdownReaderOptions metaYamlFile)
            let combinedMeta = metaUnion meta' meta
            forM (HM.elems subforest) (loadPagesRecursive combinedMeta) <&> concat

-- | Render a Page to its destination file
writePage :: SiteConfig -> Page -> IO ()
writePage config page = do
    putStrLn $ "Writing page " <> (show page.destFile)
    let options = config.htmlWriterOptions {writerTemplate = Just page.template}
    renderPandoc (writeHtml5String options) page.doc >>= writeTextFile page.destFile
