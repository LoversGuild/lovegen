-- |
-- Module      : LoveGen.Config
-- Description : Site generation configuration
-- Copyright   : Copyright (C) 2023-2024 The Lovers' Guild
-- License     : AGPL-3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides a data type for providing configuration data for
-- website generation. Each website should define this data and pass the same
-- data to all LoveGen functions that need it.
module LoveGen.Config (
    SiteConfig (..),
    defaultSiteConfig,
) where

import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Time
import System.OsPath
import Text.Pandoc

import LoveGen.Url

-- | Website generation configuration data
data SiteConfig = SiteConfig
    { -- | Base URL of the site. This has to end with "/" or anything that uses this data will break.
      siteUrl :: Url,
      -- | Directory containing Markdown pages.
      pagesDir :: OsPath,
      -- | Directory of template files
      templatesDir :: OsPath,
      -- | Static files' directory
      staticDir :: OsPath,
      -- | Name of the index file (including extension)
      indexFileName :: OsPath,
      -- | Files to read metadata from
      metaYamlFile :: OsPath,
      -- | File name of sitemap template (within templatesDir)
      sitemapTemplateFile :: OsPath,
      -- | Name of the generated sitemap file (within outputDir)
      sitemapFile :: OsPath,
      -- | Destination directory of the rendered site
      outputDir :: OsPath,
      -- | A set of required metadata keys for each page
      requiredMetadata :: HS.HashSet T.Text,
      -- | A set of optional metadata keys for each page
      optionalMetadata :: HS.HashSet T.Text,
      -- | Amount of heading levels to shift all heading elements
      shiftHeadings :: Int,
      -- | Markdown reader options
      markdownReaderOptions :: ReaderOptions,
      -- | HTML writer options
      htmlWriterOptions :: WriterOptions,
      -- | Time locale settings
      timeLocale :: TimeLocale,
      -- | Default time format string
      dateFormat :: T.Text
    }
    deriving stock (Show)

-- | Default site configuration
defaultSiteConfig :: SiteConfig
defaultSiteConfig =
    SiteConfig
        { siteUrl = "http://localhost/",
          pagesDir = [osp|pages|],
          templatesDir = [osp|templates|],
          staticDir = [osp|static|],
          indexFileName = [osp|index.html|],
          metaYamlFile = [osp|meta.yaml|],
          sitemapTemplateFile = [osp|sitemap.xml|],
          sitemapFile = [osp|sitemap.xml|],
          outputDir = [osp|output|],
          requiredMetadata =
            HS.fromList
                [ "absolute-url",
                  "date",
                  "date-meta",
                  "lang",
                  "order",
                  "site-root",
                  "site-url",
                  "template",
                  "title",
                  "url"
                ],
          optionalMetadata =
            HS.fromList
                [ "abstract",
                  "abstract-title",
                  "creation-date",
                  "creation-date-meta",
                  "description",
                  "hidden",
                  "indexing",
                  "keywords",
                  "menu-title",
                  "page-title",
                  "strings",
                  "subtitle",
                  "title-prefix",
                  "title-suffix",
                  "toc"
                ],
          shiftHeadings = 1,
          markdownReaderOptions = def {readerExtensions = pandocExtensions},
          htmlWriterOptions =
            def
                { writerEmailObfuscation = ReferenceObfuscation,
                  writerHtmlQTags = True,
                  writerNumberSections = False,
                  writerTableOfContents = True,
                  writerTemplate = Nothing,
                  writerTOCDepth = 2
                },
          timeLocale = defaultTimeLocale,
          dateFormat = "%Y-%m-%d"
        }
