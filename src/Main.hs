-- | Generate love, not war!
--
-- This is the main module of Lover's Guild's static website generator.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (ap, filterM, forM, forM_, when)
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Read qualified as T
import GHC.Stack (HasCallStack)
import System.Directory.OsPath
import System.OsPath
import Text.DocLayout (render)
import Text.Pandoc hiding (getModificationTime)
import Text.Pandoc.XML (escapeStringForXML)

import LoveGen.CommandLine
import LoveGen.Utils

-- | A trie containing data needed for menu creation
type MenuTrie = RoseTrie Url Page

-- | Main function of the generator
main :: IO ()
main = do
    cliOpts <- parseCmdLine
    rootDir <- encodeFS cliOpts.directory
    setCurrentDirectory rootDir
    buildSite finnishSite
    buildSite englishSite

-- | Build a website
buildSite :: SiteConfig -> IO ()
buildSite config = do
    pages <- loadPages config
    let menu = buildMenuTrie pages
    mapM_ (writePage config.htmlWriterOptions . addMenuToPage menu) pages
    copyStaticFiles config
    makeSitemap config pages

-- | Build a menu trie from all pages
buildMenuTrie :: [Page] -> MenuTrie
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

-- | Copy static files to their destination directories
copyStaticFiles :: SiteConfig -> IO ()
copyStaticFiles config
    = copyFilesRecursive config.staticDir config.outputDir

-- | Create a sitemap from a template
makeSitemap :: SiteConfig -> [Page] -> IO ()
makeSitemap config pages = do
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
