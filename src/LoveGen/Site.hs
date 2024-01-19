-- |
-- Module      : LoveGen.Site
-- Description : High-level functions for building web sites
-- Copyright   : Copyright (C) 2022-2024 The Lovers' Guild
-- License     : GNU Affero General Public License version 3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides high-level utilities for building a complete website.
--
-- The typical workflow is as follows:
--
--   1. Create a 'SiteConfig' object.
--   2. Determine the website's root directory.
--   3. Execute 'buildSiteInDir' with the configuration and the root directory.
--
-- If more control over the build process is needed, lower-level building blocks are also available.
module LoveGen.Site (
    -- * Building a complete website
    buildSite,
    buildSiteInDir,

    -- * Menu creation
    MenuTrie,
    buildMenuTrie,
    addMenuToPage,

    -- * Sitemap generation
    buildSiteMap,

    -- * Static file management
    copyStaticFiles,
) where

import Data.Bifunctor (second)
import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import System.Directory.OsPath
import System.OsPath
import Text.DocLayout (render)
import Text.Pandoc hiding (getModificationTime)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Writers.Shared (lookupMetaString)
import Text.Pandoc.XML (escapeStringForXML)

import LoveGen.Config
import LoveGen.Files
import LoveGen.Page
import LoveGen.Pandoc
import LoveGen.RoseTrie
import LoveGen.Url

-- | Build a complete website. This function assumes that paths referred to in
-- the provided 'SiteConfig' object are either absolute or relative to the
-- current directory. If this is not the case, use 'buildSiteInDir' instead.
buildSite :: SiteConfig -> IO ()
buildSite config = do
    pages <- loadPagesDir config
    let menu = buildMenuTrie pages
    mapM_ (writePage config . addMenuToPage menu) pages
    buildSiteMap config pages
    copyStaticFiles config

-- | Like 'buildSite' except it changes the current working directory for the
-- duration of the site building.
buildSiteInDir :: SiteConfig -> OsPath -> IO ()
buildSiteInDir config dir =
    withCurrentDirectory dir (buildSite config)

-- | A rose trie mappig url path components to 'Page's. This is used to build
-- a hierarchical menu for pages.
type MenuTrie = RoseTrie Url Page

-- | Organize the given list of pages into a hierarchy and return it as a
-- 'MenuTrie'. Hidden pages are not included. If the resulting tree would
-- contain holes, this function raises an exception.
buildMenuTrie :: HasCallStack => [Page] -> MenuTrie
buildMenuTrie = roseTrieFromList . fmap toAssoc . filter (not . (.hidden))
  where
    -- Associate a page with its breadcrumb
    toAssoc :: Page -> ([Url], Page)
    toAssoc page = (page.urlSegments, page)

-- | Add menu metadata to a page
addMenuToPage :: MenuTrie -> Page -> Page
addMenuToPage menu page =
    let pageMenu = buildMenuForPath page.urlSegments menu
    in  page
            { doc = modifyMeta (addMeta "menu" pageMenu) page.doc
            }

-- | Build a menu for a single page from the full menu trie.
--
-- The resulting value is a MetaList of all open menu levels leading to the
-- destination page. The destination page's children are included in the menu.
--
-- Each menu level in the returned list is a MetaMap which contains the
-- following keys/values:
--
--     title: The menu-title of the root page of this menu branch
--     url: URLof the root page
--     level: The depth of the root page in menu hierarchy (starting from 0).
--     items: MetaList of menu item definitions.
--
-- The menu item definitions are MetaMaps containing the following keys/values:
--
--     title: menu-title of the menu item
--     url: URL of the page referred to by this item
buildMenuForPath
    :: [Url]
    -- ^ List of page's URL segments
    -> MenuTrie
    -- ^ The full menu trie containing all pages
    -> MetaValue
buildMenuForPath initialPath initialMenu =
    MetaList $! buildMenuLevels initialPath initialMenu 0
  where
    -- \| Build menu level definitions for all open branches (following the provided path)
    buildMenuLevels :: [Url] -> MenuTrie -> Int -> [MetaValue]
    buildMenuLevels path (TrieNode root submenu) level =
        menuLevel : nextMenuLevels path
      where
        menuItems :: [MetaValue]
        menuItems = fmap toMenuItem . sortOn ((.order) . snd) . fmap (second getRoot) . HM.toList $ submenu

        -- Converta (Url, Page) pair to a proper menu item.
        toMenuItem :: (Url, Page) -> MetaValue
        toMenuItem (p, page) =
            let isCurrent = take 1 path == [p]
            in  MetaMap . M.fromList $! pageEntries isCurrent page

        menuLevel :: MetaValue
        menuLevel =
            MetaMap . M.fromList . concat $
                [ pageEntries True root,
                  [("level", MetaString . T.pack . show $! level)],
                  [("items", MetaList menuItems)]
                ]

        nextMenuLevels :: [Url] -> [MetaValue]
        nextMenuLevels [] = []
        nextMenuLevels (p : ps) =
            case HM.lookup p submenu of
                Just subtrie -> buildMenuLevels ps subtrie (level + 1)
                Nothing -> [] -- Rest of the path is probably hidden

    -- Relative URL of the root page of the whole site
    rootRelative :: Url
    rootRelative = T.replicate (length initialPath) "../"

    -- Construct menu item entries for a page
    pageEntries :: Bool -> Page -> [(Url, MetaValue)]
    pageEntries isCurrent page =
        [ ("url", MetaString $! rootRelative <> page.url),
          ("title", page.menuTitle),
          ("current", MetaBool isCurrent)
        ]

-- | Create a sitemap from a template
buildSiteMap :: SiteConfig -> [Page] -> IO ()
buildSiteMap config pages = do
    template <- loadTemplate $! config.templatesDir </> config.sitemapTemplateFile
    let metaList = fmap toSitemapMeta . sortOn (.url) . filter (not . (.hidden)) $! pages
        metaMap = M.singleton ("pages" :: T.Text) metaList
        text = render Nothing $! renderTemplate template metaMap
    writeTextFile (config.outputDir </> config.sitemapFile) text
  where
    toSitemapMeta :: Page -> M.Map T.Text T.Text
    toSitemapMeta page =
        let meta = getDocMeta $! page.doc
        in  M.fromList
                [ ("absolute-url", escapeStringForXML page.absoluteUrl),
                  ("title", escapeStringForXML $! stringify page.menuTitle),
                  ("date", escapeStringForXML $! lookupMetaString "date-meta" meta)
                ]

-- | Copy static files to their destination directories
copyStaticFiles :: SiteConfig -> IO ()
copyStaticFiles config =
    copyFilesRecursive config.staticDir config.outputDir
