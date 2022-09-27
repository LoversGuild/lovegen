-- | Generate love, not war!
--
-- This is the main module of Lover's Guild's static website generator.

module Main (main) where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (ap, filterM, void, when)
import Data.Bifunctor (second)
import Data.Binary
import Data.Bool (bool)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.Time
import Data.Time.Format.ISO8601
import Development.Shake hiding (doesFileExist)
import Development.Shake.Forward
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

import LoveGen.Orphans ()
import LoveGen.Utils

-------------------
-- Configuration --
-------------------

data SiteConfig = SiteConfig {
    -- | Base URL of the site. This has to end with "/" or anything that uses this data will break.
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
    shiftHeaders :: Int,

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
defaultSiteConfig = SiteConfig {
    siteUrl = "http://localhost/",
    pagesDir = [osp|.|],
    templatesDir = [osp|templates|],
    staticDir = [osp|static|],
    indexFileName = [osp|index.html|],
    metaYamlFile = [osp|meta.yaml|],
    sitemapTemplateFile = [osp|sitemap.xml|],
    sitemapFile = [osp|sitemap.xml|],
    outputDir = [osp|output|],
    requiredMetadata = HS.fromList [
        "absolute-url",
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
    optionalMetadata = HS.fromList [
        "abstract",
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
    shiftHeaders = 1,
    markdownReaderOptions = def { readerExtensions = pandocExtensions },
    htmlWriterOptions = def {
        writerEmailObfuscation = ReferenceObfuscation,
        writerHtmlQTags = True,
        writerNumberSections = False,
        writerTableOfContents = True,
        writerTemplate = Nothing,
        writerTOCDepth = 2
    },
    timeLocale = defaultTimeLocale,
    dateFormat = "%Y-%m-%d"
}

finnishSite :: SiteConfig
finnishSite = defaultSiteConfig {
    siteUrl = "https://rakastajienkilta.fi/",
    pagesDir = [osp|pages/fi|],
    outputDir = [osp|output/fi|],
    timeLocale = finnishTimeLocale,
    dateFormat = "%Ana %d. %Bta %Y"
}

-- | Finnish time locale
finnishTimeLocale :: TimeLocale
finnishTimeLocale = TimeLocale {
    wDays = fmap (ap (,) (take 2))
            $ [ "sunnuntai", "maanantai", "tiistai", "keskiviikko",
                "torstai", "perjantai", "lauantai" ],
    months = fmap ((,) =<< (<> "kuu"))
             $ [ "tammi", "helmi", "maalis", "huhti", "touko", "kesä",
                 "heinä", "elo", "syys", "loka", "marras", "joulu" ],
    amPm = ("ennen puoltapäivää", "puolen päivän jälkeen"),
    dateTimeFmt = "%d.%m.%Y %H:%M:%S",
    dateFmt = "%d.%m.%Y",
    timeFmt = "%H:%M:%S",
    time12Fmt = "%H:%M:%S",
    knownTimeZones = []
}

englishSite :: SiteConfig
englishSite = defaultSiteConfig {
    siteUrl = "https://loversguild.org/",
    pagesDir = [osp|pages/en|],
    outputDir = [osp|output/en|],
    dateFormat = "on %a, %d %b %Y"
}

--------------------------
-- End of Configuration --
--------------------------

-- | Data stored off a single page
data Page = Page {
    -- | URL fragment
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
    deriving anyclass (Binary)

-- | A trie containing data needed for menu creation
type MenuTrie = RoseTrie Url Page

-- | Main function of the generator
main :: IO ()
main = shakeArgsForward shakeOpts do
    buildSite finnishSite
    buildSite englishSite
  where
    shakeOpts :: ShakeOptions
    shakeOpts = shakeOptions {
                    shakeFiles = "shake",
                    shakeLint = Just LintBasic,
                    shakeLintInside = ["."],
                    shakeThreads = 0,
                    shakeVerbosity = Chatty
                }

-- | Build a website
buildSite :: SiteConfig -> Action ()
buildSite config = do
    pages <- loadPages config
    let menu = buildMenuTrie pages
    mapM_ (writePage config . addMenuToPage menu) pages
    copyStaticFiles config
    makeSitemap config pages

-- | Load all pages
loadPages :: HasCallStack => SiteConfig -> Action [Page]
loadPages config = do
    -- Build a tree from the directory hierarchy.
    pathTrie <- makeDirectoryTrie config.pagesDir
    loadPagesRecursive nullMeta pathTrie
  where
    loadPagesRecursive :: Meta -> RoseTrie OsPath OsPath -> Action [Page]
    loadPagesRecursive meta (TrieNode path subforest)
        | HM.null subforest = do
              -- This might be a file = do
              isFile <- liftIO $! doesFileExist path
              if isFile && isExtensionOf [osp|md|] path
                  then fmap (: []) $! loadPage config meta path
                  else pure []
        | otherwise = do
              -- This is a directory
              let metaYamlFile = path </> config.metaYamlFile
              meta' <- liftIO (doesFileExist metaYamlFile)
                  >>= bool (pure nullMeta) (loadYamlMeta config metaYamlFile)
              let combinedMeta = metaUnion meta' meta
              forP (HM.elems subforest) (loadPagesRecursive combinedMeta) >>= pure . concat

-- | Build a menu trie from all pages
buildMenuTrie :: [Page] -> MenuTrie
buildMenuTrie = roseTrieFromList . fmap toAssoc . filter (not . (.hidden))
  where
    -- Associate a page with its breadcrumb
    toAssoc :: Page -> ([Url], Page)
    toAssoc page = (page.urlSegments, page)

-- | Add menu metadata to a page
addMenuToPage :: MenuTrie -> Page -> Page
addMenuToPage menu page
    = let pageMenu = buildMenuForPath page.urlSegments menu
      in page {
             doc = modifyMeta (addMeta "menu" pageMenu) page.doc
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
--
buildMenuForPath
    :: [Url] -- ^ List of page's URL segments
    -> MenuTrie -- ^ The full menu trie containing all pages
    -> MetaValue
buildMenuForPath initialPath initialMenu
    = MetaList $! buildMenuLevels initialPath initialMenu 0
  where
    -- | Build menu level definitions for all open branches (following the provided path)
    buildMenuLevels :: [Url] -> MenuTrie -> Int -> [MetaValue]
    buildMenuLevels path (TrieNode root submenu) level
        = menuLevel : nextMenuLevels path
      where
        menuItems :: [MetaValue]
        menuItems = fmap toMenuItem . sortOn ((.order) . snd) . fmap (second getRoot) . HM.toList $ submenu

        -- Converta (Url, Page) pair to a proper menu item.
        toMenuItem :: (Url, Page) -> MetaValue
        toMenuItem (p, page)
            = let isCurrent = take 1 path == [p]
              in MetaMap . M.fromList $! pageEntries isCurrent page

        menuLevel :: MetaValue
        menuLevel
            = MetaMap . M.fromList . concat $
              [ pageEntries True root,
                [ ("level", MetaString . T.pack . show $! level) ],
                [ ("items", MetaList menuItems) ]
              ]

        nextMenuLevels :: [Url] -> [MetaValue]
        nextMenuLevels [] = []
        nextMenuLevels (p : ps)
            = case HM.lookup p submenu of
                  Just subtrie -> buildMenuLevels ps subtrie (level + 1)
                  Nothing -> error $ "Unknown menu item " <> (show p) <> " in menu path " <> (show initialPath)

    -- Relative URL of the root page of the whole site
    rootRelative :: Url
    rootRelative = T.replicate (length initialPath) "../"

    -- Construct menu item entries for a page
    pageEntries :: Bool -> Page -> [(Url, MetaValue)]
    pageEntries isCurrent page
        = [ ("url", MetaString $! rootRelative <> page.url),
            ("title", page.menuTitle),
            ("current", MetaBool isCurrent)
          ]

-- | Load a single page from a Markdown file
loadPage :: HasCallStack => SiteConfig -> Meta -> OsPath -> Action Page
loadPage config meta fp = cacheAction ("page" :: T.Text, fp) do
    liftIO . putStrLn $ "Loading page " <> (show fp)
    doc <- readMarkdownFile config fp >>= pure . headerShift config.shiftHeaders

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
            pure $! (urlSlashed, dest')
        Nothing -> do
            let dest' = joinPath . drop pagesDirSegmentsCount . splitDirectories . dropExtension $! fp
            url' <- osPathToUrl dest'
            pure $! (url' <> "/", dest')

    let destFile = normalise $! config.outputDir </> baseDestFile </> config.indexFileName
        urlSegments = filter (not . T.null) . T.split (== '/') $! url
        absoluteUrl = config.siteUrl <> url
        rootUrlRelative =
            if null urlSegments
            then "./"
            else T.replicate (length urlSegments) "../"

    time <- fetchLastCommitTime fp >>= \case
        Just t -> pure $! t
        Nothing -> liftIO $! getModificationTime fp
    creationTime <- fetchFirstCommitTime fp >>= pure . fromMaybe time

    let doc' = flip modifyMeta doc $
               \m -> addMeta "site-root" (MetaString rootUrlRelative)
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

    let menuTitle = fromMaybe (error "Impossible: page has no title")
                    $ lookupMeta "menu-title" meta'
                    <|> lookupMeta "title" meta'
        order = either error fst . T.decimal . stringify . lookupMetaForce "order" $ meta'
        hidden = case lookupMeta "hidden" meta' of
                     Just (MetaBool val) -> val
                     Nothing -> False
                     val -> error $ "Invalid value for meta key 'hidden': " <> (show val)

    templateFile <- (liftIO . encodeFS) . T.unpack . stringify . lookupMetaForce "template" $ meta'
    template <- loadTemplate $! config.templatesDir </> templateFile

    pure $! Page {
        url = url,
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
loadTemplate :: HasCallStack => OsPath -> Action (Template T.Text)
loadTemplate fp = cacheAction ("template" :: T.Text, fp) do
    stringFP <- liftIO $! decodeFS fp
    text <- readTextFile fp
    compileTemplate stringFP text
        >>= either fail pure

-- | Load metadata from YAML file
loadYamlMeta :: SiteConfig -> OsPath -> Action Meta
loadYamlMeta config fp = do
    stringFP <- liftIO $! decodeFS fp
    readBinaryFile fp >>= runPandocAction . (yamlToMeta config.markdownReaderOptions (Just stringFP))

-- | Render a Page to its destination file
writePage :: SiteConfig -> Page -> Action ()
writePage config page = cacheActionWith ("writePage" :: T.Text, page.destFile) page do
    liftIO . putStrLn $ "Writing page " <> (show page.destFile)
    let options = config.htmlWriterOptions { writerTemplate = Just page.template }
    renderPandoc (writeHtml5String options) page.doc >>= writeTextFile page.destFile

-- | Load a markdown file into a pandoc document
readMarkdownFile :: SiteConfig -> OsPath -> Action Pandoc
readMarkdownFile config fp
    = readTextFile fp >>= readPandoc (readMarkdown config.markdownReaderOptions) fp

-- | Copy static files to their destination directories
copyStaticFiles :: SiteConfig -> Action ()
copyStaticFiles config = do
    let staticSegmentsCount = length . splitDirectories $ config.staticDir
    files <- liftIO $! listDirectoryRecursive config.staticDir
             >>= filterM (liftIO . doesFileExist)
    void $! forP files \sourceFile ->
        let pathSegments = drop staticSegmentsCount $! splitDirectories sourceFile
            destFile = joinPath $! config.outputDir : pathSegments
        in copyFileIfChanged sourceFile destFile
    pure ()

-- | Copy a single file creating missing directories as necessary. The copy is
-- not performed, if destination file exists, and its size and modification
-- time match the source file.
copyFileIfChanged :: OsPath -- ^ Source file path
         -> OsPath -- ^ Destination file path
         -> Action ()
copyFileIfChanged source dest = liftIO $ do
    -- Check if the file needs to be copied
    doCopy <- ifM (not <$> doesFileExist dest) (pure True)
              $ ifM (liftA2 (/=) (getFileSize source) (getFileSize dest)) (pure True)
              $ ifM (liftA2 (/=) (getModificationTime source) (getModificationTime dest)) (pure True)
              $ (pure False)

    when doCopy $ do
        putStrLn $ "Copying file " <> (show source)
        createDirectoryIfMissing True (takeDirectory dest)
        copyFileWithMetadata source dest

-- | Create a sitemap from a template
makeSitemap :: SiteConfig -> [Page] -> Action ()
makeSitemap config pages = do
    template <- loadTemplate $! config.templatesDir </> config.sitemapTemplateFile
    let metaList = fmap toSitemapMeta . sortOn (.url) . filter (not . (.hidden)) $! pages
        metaMap = M.singleton ("pages" :: T.Text) metaList
        text = render Nothing $! renderTemplate template metaMap
    writeTextFile (config.outputDir </> config.sitemapFile) text
  where
    toSitemapMeta :: Page -> M.Map T.Text T.Text
    toSitemapMeta page
        = let meta = getDocMeta $! page.doc
          in M.fromList
             [ ("absolute-url", escapeStringForXML page.absoluteUrl),
               ("title", escapeStringForXML $! stringify page.menuTitle),
               ("date", escapeStringForXML $! lookupMetaString "date-meta" meta)
             ]

-- | Make a RoseTrie from a directory tree
makeDirectoryTrie :: OsPath -> Action (RoseTrie OsPath OsPath)
makeDirectoryTrie rootDir
    = let prefixLength = length $! splitDirectories rootDir
      in liftIO (listDirectoryRecursive rootDir)
         >>= pure . roseTrieFromList . fmap (\p -> (drop prefixLength $! splitDirectories p, p))
