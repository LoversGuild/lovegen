-- |
-- Module      : LoveGen.RoseTrie
-- Description : A prefix trie with associated values and arbitrary number of child nodes
-- Copyright   : Copyright (C) 2023-2024 The Lovers' Guild
-- License     : AGPL-3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines Rose tries.
--
-- A 'RoseTrie' represents a hybrid tree structure that combines elements of
-- a rose tree and a trie (prefix tree). Each 'RoseTrie' node stores a value of
-- type 'a' and has an associated 'RoseForest', which is a hash map from keys
-- of type 'k' to subsequent 'RoseTrie' nodes. This allows for a flexible number
-- of children per node (as in a rose tree), while also enabling efficient
-- prefix-based retrieval of values (as in a trie).
--
-- The 'RoseTrie' can be visualized as a tree where every path from the root
-- to a specific node represents a sequence of keys, and the node itself
-- encapsulates a value. The organization of nodes into a hash map allows for
-- efficient navigation based on given keys, supporting operations that are
-- typical for trie structures, such as efficient lookup, insertion, and deletion
-- of key sequences.
--
-- The 'RoseTrie' is a versatile data structure suitable for various applications
-- that require organizing and retrieving data hierarchically and via key sequences,
-- such as indexing systems, associative arrays, and complex nested configurations.
--
-- @
-- data RoseTrie k a = TrieNode { value :: a, subForest :: RoseForest k a }
-- type RoseForest k a = HM.HashMap k (RoseTrie k a)
-- @
module LoveGen.RoseTrie (
    RoseTrie (..),
    RoseForest,
    singletonRoseTrie,
    roseTrieFromList,
    getRoot,
    getForest,
    insertWithPath,
)
where

import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.List (foldl', sortOn)
import GHC.Stack

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
    :: forall a k
     . (HasCallStack, Hashable k, Show k, Show a)
    => [([k], a)]
    -> RoseTrie k a
roseTrieFromList = roseTrieFromSortedList . sortOn (length . fst)
  where
    -- Extract root element from the sorted (path, item) list, construct the
    -- top node from it and pass the rest to the rose forest builder
    roseTrieFromSortedList :: HasCallStack => [([k], a)] -> RoseTrie k a
    roseTrieFromSortedList [] = error "Cannot create rose trie from empty item list."
    roseTrieFromSortedList (([], item) : rest) =
        TrieNode item $! rosesFromList rest
    roseTrieFromSortedList list = error $ "No trie root element found among " <> (show $! fmap fst list)

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
    => [k]
    -> a
    -> RoseForest k a
    -> RoseForest k a
insertWithPath [p] node forest = HM.insert p (singletonRoseTrie node) forest
insertWithPath (p : ps) node forest =
    case HM.lookup p forest of
        Just (TrieNode subnode subforest) ->
            HM.insert p (TrieNode subnode $! insertWithPath ps node subforest) forest
        Nothing ->
            error $!
                "Attempting to insert a node into a RoseTrie with too long path "
                    <> (show $! p : ps)
                    <> " -- map is "
                    <> show forest
insertWithPath [] _ _ = error $! "Attempted to add an empty key into a RoseTrie"
