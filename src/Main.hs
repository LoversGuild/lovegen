-- | Generate love, not war!
--
-- This is the main module of Lover's Guild's static website generator.

module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Binary
import Data.Bool (bool)
import Data.HashSet qualified as HS
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict qualified as HM
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
import Text.Pandoc hiding (getModificationTime)
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import Text.Pandoc.Shared (headerShift, stringify)

import LoveGen.Orphans ()
import LoveGen.Utils

-------------------
-- Configuration --
-------------------

data SiteConfig = SiteConfig {
    -- | Directory containing Markdown pages.
    pagesDir :: OsPath,

    -- | Directory of template files
    templatesDir :: OsPath,

    -- | Name of the index file (without extension)
    indexFileName :: OsPath,

    -- | Files to read metadata from
    metaYamlFile :: OsPath,

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
    htmlWriterOptions :: WriterOptions
    }
    deriving stock (Show)

-- | Default site configuration
defaultSiteConfig :: SiteConfig
defaultSiteConfig = SiteConfig {
    pagesDir = [osp|.|],
    templatesDir = [osp|templates|],
    indexFileName = [osp|index|],
    metaYamlFile = [osp|meta.yaml|],
    outputDir = [osp|output|],
    requiredMetadata = HS.fromList [
        "date",
        "date-meta",
        "lang",
        "order",
        "site-root",
        "template",
        "title"
    ],
    optionalMetadata = HS.fromList [
        "abstract",
        "description",
        "keywords",
        "menu-title",
        "page-title",
        "strings",
        "subtitle",
        "title-prefix",
        "title-suffix"
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
    }
}

finnishSite :: SiteConfig
finnishSite = defaultSiteConfig {
    pagesDir = [osp|pages/fi|],
    outputDir = [osp|output/fi|]
    }

englishSite :: SiteConfig
englishSite = defaultSiteConfig {
    pagesDir = [osp|pages/en|],
    outputDir = [osp|output/en|]
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

    -- | Contents and metadata
    doc :: Pandoc,

    -- | Title of the page in a menu
    menuTitle :: MetaValue,

    -- | Sorting score for the page in a menu
    order :: Int,

    -- | Template that is to be used to render this page
    template :: Template T.Text,

    -- | Name of the destination file
    destFile :: OsPath
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Binary)

-- | A trie containing data needed for menu creation
type MenuMap = RoseForest Url Page

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
    let menu = buildMenu pages
    mapM_ (writePage config . addMenuToPage menu) pages

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
buildMenu :: [Page] -> MenuMap
buildMenu = rosesFromList . fmap toMenuItem . filter (not . null . (.urlSegments))
  where
    -- Turn a page into its menu item
    toMenuItem :: Page -> ([Url], Page)
    toMenuItem page = (page.urlSegments, page)

-- | Add menu metadata to a page
addMenuToPage :: MenuMap -> Page -> Page
addMenuToPage menu page
    = let pageMenu = buildMenuForPath page.urlSegments menu (length page.urlSegments) 1
      in page {
             doc = modifyMeta (addMeta "menu" pageMenu) page.doc
         }

-- | Build a menu for a single page from the full menu trie
buildMenuForPath
    ::  [Url] -- ^ List of page's URL fragments
    -> MenuMap -- ^ The full menu trie containing all pages
    -> Int -- ^ Menu depth
    -> Int -- ^ Current rendering level
    -> MetaValue -- ^ The resulting cut down version of the menu as Pandoc metadata map
buildMenuForPath paths menu depth level
    = let menuItems = fmap (uncurry buildMenuItems) . sortOn (getOrder . snd) . HM.toList $ menu
      in if null menuItems
         then MetaString "" -- Unfortunately empty maps are considered to be True in doctemplates conditionals
         else MetaMap . M.fromList $
              [ ("level", MetaString . T.pack . show $! level),
                ("items", MetaList $ menuItems)
              ]
  where
    buildMenuItems :: Url -> RoseTrie Url Page -> MetaValue
    buildMenuItems key (TrieNode page submenu)
        = MetaMap . M.fromList $!
          [ ("url", MetaString $! (T.replicate depth "../") <> page.url),
            ("title", page.menuTitle),
            ("submenu", if descend key
                        then buildMenuForPath (tail paths) submenu depth (level + 1)
                        else MetaString ""
            )
          ]

    -- Get the sorting order of a Trie node
    getOrder :: RoseTrie a Page -> Int
    getOrder (TrieNode page _) = page.order

    -- Find out whether to descend into a submenu
    descend :: Url -> Bool
    descend url
        | null paths = False
        | otherwise = head paths == url

-- | Load a single page from a Markdown file
loadPage :: HasCallStack => SiteConfig -> Meta -> OsPath -> Action Page
loadPage config meta fp = cacheAction ("page" :: T.Text, fp) do
    liftIO . putStrLn $ "Loading page " <> (show fp)
    doc <- readMarkdownFile config fp >>= pure . headerShift config.shiftHeaders

    -- Metadata handling
    let pagesDirSegments = splitDirectories $! config.pagesDir
        pagesDirSegmentsCount = length pagesDirSegments
        (contentDirs, pathSegments) = splitAt pagesDirSegmentsCount $! splitDirectories . dropExtension $! fp
        destPathSegments =
            if last pathSegments == config.indexFileName
            then init pathSegments
            else pathSegments
        destFile = config.outputDir </> (joinPath destPathSegments) </> config.indexFileName <.> [osp|html|]

    unless (contentDirs == pagesDirSegments) $ fail $! "Page file name " <> (show fp) <> " does not start with page directory " <> (show config.pagesDir)
    urlSegments <- mapM osPathToUrl destPathSegments
    let url = if null urlSegments
            then "."
            else T.intercalate "/" urlSegments
        rootUrlRelative =
            if null urlSegments
            then "./"
            else T.replicate (length urlSegments) "../"
    
    time <- fetchLastCommitTime fp >>= \case
        Just t -> pure $! t
        Nothing -> liftIO $! getModificationTime fp

    let doc' = flip modifyMeta doc $
               \m -> addMeta "site-root" (MetaString rootUrlRelative)
                     . addMeta "date-meta" (MetaString . T.pack $! iso8601Show time)
                     . addMeta "date" (MetaString . T.pack $! formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time)
                     $! metaUnion m meta
        meta' = getDocMeta doc'

    verifyMetaKeys config.requiredMetadata config.optionalMetadata meta'

    let menuTitle = fromMaybe (error "Impossible: page has no title")
                    $ lookupMeta "menu-title" meta'
                    <|> lookupMeta "title" meta'
        order = either error fst . T.decimal . stringify . lookupMetaForce "order" $ meta'

    templateFile <- (liftIO . encodeFS) . T.unpack . stringify . lookupMetaForce "template" $ meta'
    template <- loadTemplate $! config.templatesDir </> templateFile

    pure $! Page {
        url = url,
        urlSegments = urlSegments,
        doc = doc',
        menuTitle = menuTitle,
        order = order,
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

-- | Make a RoseTrie from a directory tree
makeDirectoryTrie :: OsPath -> Action (RoseTrie OsPath OsPath)
makeDirectoryTrie rootDir = liftIO $! listDirectoryRecursive rootDir >>= pure . buildTrie (length $! splitDirectories rootDir)
  where
    buildTrie :: Int -> [OsPath] -> RoseTrie OsPath OsPath
    buildTrie _ [] = error $ "No directory entries"
    buildTrie prefixLength (rootDir' : paths)
        = if equalFilePath rootDir rootDir'
          then TrieNode rootDir'
               $! rosesFromList
               . fmap (\p -> (drop prefixLength $! splitDirectories p, p))
               $! paths
          else error $ "listDirectoryRecursive did not return the root dir " <> (show rootDir)
               <> " as the first element, but " <> (show rootDir')

