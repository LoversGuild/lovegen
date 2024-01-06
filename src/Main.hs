-- | Generate love, not war!
--
-- This is the main module of Lover's Guild's static website generator.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (ap, filterM, forM, forM_, when)
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Directory.OsPath
import System.OsPath
import Text.DocLayout (render)
import Text.Pandoc hiding (getModificationTime)
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import Text.Pandoc.Shared (headerShift, stringify)
import Text.Pandoc.Writers.Shared (lookupMetaString)
import Text.Pandoc.XML (escapeStringForXML)

import LoveGen.CommandLine
import LoveGen.Utils

-- | Data stored off a single page
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
      -- | Is this page hidden from menus
      hidden :: Bool,
      -- | Template that is to be used to render this page
      template :: Template T.Text,
      -- | Name of the destination file
      destFile :: OsPath
    }
    deriving stock (Eq, Generic, Show)

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
    mapM_ (writePage config . addMenuToPage menu) pages
    copyStaticFiles config
    makeSitemap config pages

-- | Load all pages
loadPages :: HasCallStack => SiteConfig -> IO [Page]
loadPages config = do
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
                    >>= bool (pure nullMeta) (loadYamlMeta config metaYamlFile)
            let combinedMeta = metaUnion meta' meta
            forM (HM.elems subforest) (loadPagesRecursive combinedMeta) <&> concat

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

-- | Load a single page from a Markdown file
loadPage :: HasCallStack => SiteConfig -> Meta -> OsPath -> IO Page
loadPage config meta fp = do
    putStrLn $ "Loading page " <> show fp
    doc <- readMarkdownFile config fp <&> (headerShift config.shiftHeaders)

    -- Metadata handling
    let pagesDirSegments = splitDirectories $! config.pagesDir
        pagesDirSegmentsCount = length pagesDirSegments

    -- Calculate relative url and base target file name either based on the
    -- page file name or the "url" metadata field. Avoid extraneous
    -- encoding/decoding of the file name.
    (url, baseDestFile) <- case lookupMeta "url" $! getDocMeta doc of
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

    let doc' = flip modifyMeta doc $
            \m ->
                addMeta "site-root" (MetaString rootUrlRelative)
                    . addMeta "site-url" (MetaString config.siteUrl)
                    . addMeta "absolute-url" (MetaString absoluteUrl)
                    . addMeta "url" (MetaString if T.null url then "./" else url)
                    . addMeta "date-meta" (MetaString . T.pack $! iso8601Show time)
                    . addMeta "date" (MetaString . T.pack $! formatTime config.timeLocale (T.unpack config.dateFormat) time)
                    . addMeta "creation-date-meta" (MetaString . T.pack $! iso8601Show creationTime)
                    . addMeta "creation-date" (MetaString . T.pack $! formatTime config.timeLocale (T.unpack config.dateFormat) creationTime)
                    $! metaUnion m meta
        meta' = getDocMeta doc'

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
              order = order,
              hidden = hidden,
              template = template,
              destFile = destFile
            }

-- | Load a template from file
loadTemplate :: HasCallStack => OsPath -> IO (Template T.Text)
loadTemplate fp = do
    stringFP <- decodeFS fp
    text <- readTextFile fp
    compileTemplate stringFP text
        >>= either fail pure

-- | Load metadata from YAML file
loadYamlMeta :: SiteConfig -> OsPath -> IO Meta
loadYamlMeta config fp = do
    stringFP <- decodeFS fp
    readBinaryFile fp >>= runPandoc . (yamlToMeta config.markdownReaderOptions (Just stringFP))

-- | Render a Page to its destination file
writePage :: SiteConfig -> Page -> IO ()
writePage config page = do
    putStrLn $ "Writing page " <> (show page.destFile)
    let options = config.htmlWriterOptions {writerTemplate = Just page.template}
    renderPandoc (writeHtml5String options) page.doc >>= writeTextFile page.destFile

-- | Load a markdown file into a pandoc document
readMarkdownFile :: SiteConfig -> OsPath -> IO Pandoc
readMarkdownFile config fp =
    readTextFile fp >>= readPandoc (readMarkdown config.markdownReaderOptions) fp

-- | Copy static files to their destination directories
copyStaticFiles :: SiteConfig -> IO ()
copyStaticFiles config = do
    let staticSegmentsCount = length . splitDirectories $ config.staticDir
    files <-
        listDirectoryRecursive config.staticDir
            >>= filterM doesFileExist
    forM_ files \sourceFile ->
        let pathSegments = drop staticSegmentsCount $! splitDirectories sourceFile
            destFile = joinPath $! config.outputDir : pathSegments
        in  copyFileIfChanged sourceFile destFile

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
