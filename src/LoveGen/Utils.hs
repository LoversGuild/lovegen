{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

-- | Utility functions for the website genrator
--
-- These are generic routines and not directly related to any particular website.

module LoveGen.Utils
    (
        -- URLs
        Url,
        osPathToUrl,
        urlToOsPath,

        -- Monadic conditional
        ifM,

        -- * File management
        readTextFile,
        writeTextFile,
        readBinaryFile,
        writeBinaryFile,
        listDirectoryRecursive,

        -- * Pandoc format conversion
        PandocReader,
        PandocWriter,
        readPandoc,
        renderPandoc,
        runPandocAction,

        -- * Metadata management
        verifyMetaKeys,
        getDocMeta,
        modifyMeta,
        metaUnion,
        addMeta,
        lookupMetaForce,

        -- * RoseTrie
        RoseTrie (..),
        RoseForest,
        singletonRoseTrie,
        roseTrieFromList,
        getRoot,
        getForest,
        insertWithPath,

        -- * Time information management
        fetchFirstCommitTime,
        fetchLastCommitTime
    )
where

import Control.Monad (foldM, when, (>=>))
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Hashable
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (foldl', sortOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Time
import Data.Time.Format.ISO8601
import Development.Shake (Action, liftIO, need)
import GHC.Stack
import System.Directory.OsPath
import System.File.OsPath qualified as OP
import System.OsPath
import System.Process.Typed
import Text.DocTemplates (TemplateMonad (..))
import Text.Pandoc

-- | Type for storing URLs
type Url = T.Text

-- | Convert an OsPath to Url
osPathToUrl :: OsPath -> Action Url
osPathToUrl = liftIO . decodeFS >=> pure . T.pack

-- | Convert an Url to OsPath
urlToOsPath :: Url -> Action OsPath
urlToOsPath url
    | T.null url = pure $! [osp|.|]
    | otherwise = liftIO . encodeFS . T.unpack $! url

-- | Like if, but condition can be monadic
ifM :: Monad m => m Bool -- ^ Conditional
    -> m a -- ^ Then action
    -> m a -- ^ Else action
    -> m a
ifM cond true false = cond >>= bool false true

-- | Read a text file (in UTF-8) within the shake Action monad.
--
-- Throws an exception on invalid UTF-8 input
readTextFile :: HasCallStack => OsPath -> Action T.Text
readTextFile = fmap decodeUtf8 . readBinaryFile

-- | Write a text file with UTF-8 encoding
writeTextFile :: HasCallStack => OsPath -> T.Text -> Action ()
writeTextFile fp text = writeBinaryFile fp $! encodeUtf8 text

-- | Read a file of bytes within the shake Action monad.
readBinaryFile :: HasCallStack => OsPath -> Action BS.ByteString
readBinaryFile fp = do
    liftIO (decodeFS fp) >>= need . (: [])
    liftIO $! OP.readFile' fp

-- | Write a file of bytes as a Shake action. Create target directories, as needed.
writeBinaryFile :: HasCallStack => OsPath -> BS.ByteString -> Action ()
writeBinaryFile fp bytes
    = liftIO $! do
          createDirectoryIfMissing True $! takeDirectory fp
          doesFileExist fp >>= flip when (removeFile fp)
          OP.writeFile' fp bytes

-- | Recursively list all paths under a subdirectory.
listDirectoryRecursive :: OsPath -> IO [OsPath]
listDirectoryRecursive dir = fmap reverse $! scanDir [dir] dir
  where
    -- Scan a subdirectory and return all paths found so far
    scanDir :: [OsPath] -> OsPath -> IO [OsPath]
    scanDir found fp
        = listDirectory fp
          >>= foldM scanEntry found . fmap (fp </>)

    -- Scan a single directory entry and return a list of all paths found so far
    scanEntry :: [OsPath] -> OsPath -> IO [OsPath]
    scanEntry found path
        = let found' = path : found
          in doesDirectoryExist path
             >>= bool (pure $! found') (scanDir found' path)

-- | A reader function. Make one by applying a Pandoc reader to ReaderOptions.
type PandocReader = [(FilePath, T.Text)] -> PandocIO Pandoc

-- | A writer function. Make one by applying a Pandoc writer to WriterOptions.
type PandocWriter = Pandoc -> PandocIO T.Text

-- | Read a document with a specified reader function
readPandoc
    :: PandocReader -- ^ Reader to use for converting the text to AST
    -> OsPath -- ^ Name of the file (for error messages)
    -> T.Text -- ^ Text to convert
    -> Action Pandoc
readPandoc reader fp content = do
    stringFP <- liftIO $! decodeFS fp
    runPandocAction $! reader [(stringFP, content)]

-- | Render a Pandoc document with a writer function
renderPandoc
    :: PandocWriter
    -> Pandoc
    -> Action T.Text
renderPandoc writer doc
    = runPandocAction $! writer doc

-- | Run Pandoc inside shake
runPandocAction :: PandocIO a -> Action a
runPandocAction
    = (liftIO . runIO >=> either (fail . show) pure)

-- metaToJson :: PandocWriter -> Meta -> Action A.Value
-- metaToJson writer (Meta meta)
--     = runPandocAction $! A.toJSON <$> traverse go meta
--   where
--     go :: MetaValue -> PandocIO A.Value
--     go (MetaMap m) = A.toJSON <$> traverse go m
--     go (MetaList m) = A.toJSONList <$> traverse go m
--     go (MetaBool m) = pure $! A.toJSON m
--     go (MetaString m) = pure $! A.toJSON m
--     go (MetaInlines m) = A.toJSON <$> (writer . Pandoc mempty . (: []) . Plain $! m)
--     go (MetaBlocks m) = A.toJSON <$> (writer . Pandoc mempty $! m)

-- An orphan isntance for TemplateMonad to be able to track partial template
-- loads.
instance TemplateMonad Action where
    getPartial :: FilePath -> Action T.Text
    getPartial = liftIO . encodeFS >=> readTextFile

-- | Verify that a JSON metadata object has all desired keys, and all of them
verifyMetaKeys
    :: HasCallStack
    => HS.HashSet T.Text -- ^ A set of mndatory keys
    -> HS.HashSet T.Text -- ^ A set of optional keys
    -> Meta  -- ^ The metadata object
    -> Action ()
verifyMetaKeys required optional (Meta mmap)
    = let present = HS.fromList $! M.keys mmap
          desired = HS.union optional required
          missing = HS.difference required present
          surplus = HS.difference present desired
      in if not $! HS.null missing && HS.null surplus
         then error $! "Metadata verification failed -"
              <> " missing keys: " <> (show missing)
              <> ", excess keys: " <> (show surplus)
         else pure $! ()

-- | Extract metadata from a Pandoc document
getDocMeta :: Pandoc -> Meta
getDocMeta (Pandoc meta _) = meta

-- | Modify the metadata embedded in a Pandoc
modifyMeta :: (Meta -> Meta) -> Pandoc -> Pandoc
modifyMeta f (Pandoc meta blocks)
    = Pandoc (f meta) blocks

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
lookupMetaForce key meta
    = case lookupMeta key meta of
          Just val -> val
          Nothing ->
              error $ "Required metadata key " <> (show key) <> " was not found from metadata: " <> (show meta)

-- | Rose trie and map data structure using hashmaps
data RoseTrie k a = TrieNode a (RoseForest k a)
    deriving stock (Eq, Show)

-- | A forest trie built using a HashMap
type RoseForest k a = HM.HashMap k (RoseTrie k a)

-- | Make a singleton trie
singletonRoseTrie :: a -> RoseTrie k a
singletonRoseTrie node = TrieNode node HM.empty

-- | Convert a list of paths to nods into a RoseTrie
roseTrieFromList
    :: forall a k. (HasCallStack, Hashable k, Show k, Show a)
    => [([k], a)] -> RoseTrie k a
roseTrieFromList = roseTrieFromSortedList . sortOn (length . fst)
  where
    -- Extract root element from the sorted (path, item) list, construct the
    -- top node from it and pass the rest to the rose forest builder
    roseTrieFromSortedList :: HasCallStack => [([k], a)] -> RoseTrie k a
    roseTrieFromSortedList [] = error $ "Cannot create rose trie from empty item list."
    roseTrieFromSortedList (([], item) : rest)
        = TrieNode item $! rosesFromList rest
    roseTrieFromSortedList list = error $ "No trie root element found among " <> (show $! map fst list)

    rosesFromList :: HasCallStack => [([k], a)] -> RoseForest k a
    rosesFromList = foldl' (flip $ uncurry insertWithPath) HM.empty

-- | Extract the root item from a rose trie
getRoot :: RoseTrie k a -> a
getRoot (TrieNode root _) = root

-- | Extract the forest from a rose trie
getForest :: RoseTrie k a -> RoseForest k a
getForest (TrieNode _ forest) = forest

-- | Insert a node to a forest with path
insertWithPath
    :: (HasCallStack, Hashable k, Show k, Show a)
    => [k] -> a -> RoseForest k a -> RoseForest k a
insertWithPath [p] node forest = HM.insert p (singletonRoseTrie node) forest
insertWithPath (p : ps) node forest
    = case HM.lookup p forest of
          Just (TrieNode subnode subforest) ->
              HM.insert p (TrieNode subnode $! insertWithPath ps node subforest) forest
          Nothing ->
              error $! "Attempting to insert a node into a RoseTrie with too long path "
              <> (show $! p : ps) <> " -- map is "
              <> (show forest)
insertWithPath [] _ _ = error $! "Attempted to add an empty key into a RoseTrie"

-- | Fetch a commit datetime of a file from git repository
fetchGitCommitTime :: [String] -> OsPath -> Action (Maybe UTCTime)
fetchGitCommitTime gitOpts fp = do
    stringFP <- liftIO $! decodeFS fp
    (code, out) <- readProcessStdout $! setEnv [("TZ", "UTC")] $! proc "git" (["log", "-1", "--format=%aI" ] <> gitOpts <> [ "--", stringFP ])
    if code == ExitSuccess
        then pure $! Nothing
        else pure $! iso8601ParseM . T.unpack . decodeUtf8 . BL.toStrict $! out

-- | Fetch the date and time of the first commit of a file
fetchFirstCommitTime :: OsPath -> Action (Maybe UTCTime)
fetchFirstCommitTime = fetchGitCommitTime [ "--diff-filter=A", "--follow", "--find-renames=40%" ]

-- | Fetch the date and time of the latest commit of a file
fetchLastCommitTime :: OsPath -> Action (Maybe UTCTime)
fetchLastCommitTime = fetchGitCommitTime []
