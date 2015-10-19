{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Data.Recall
-- Copyright   :  Nick Spinale 2015
-- License     :  MIT
--
-- Maintainer  :  nickspinale@carleton.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- A typeclass for generic memoization.
-- Inspired by <http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf>.
--

module Data.Recall
    (
    -- * 'Memo' Typeclass
      Memo
    , memoize
    -- * Generic memoization functions
    , asArray
    , asEnumTree
    , asTree
    ) where

import Data.Array


-- | Represents an approach to memoizing functions from a given type.
class Memo a where

    -- | Data structure for storing evaluated bits.
    data Table a :: * -> *

    -- | Lazily fill table.
    toTable :: (a -> b) -> Table a b

    -- | Access table.
    fromTable :: Table a b -> a -> b

------------------
-- Basic instances
------------------

instance Memo Bool where
    data Table Bool b = TBool b b
    toTable f = TBool (f True) (f False)
    fromTable (TBool t _) True = t
    fromTable (TBool _ f) False = f

instance Memo a => Memo (Maybe a) where
    data Table (Maybe a) b = TMaybe b (Table a b)
    toTable f = TMaybe (f Nothing) $ toTable (f . Just)
    fromTable (TMaybe n _) Nothing = n
    fromTable (TMaybe _ j) (Just r) = fromTable j r

instance (Memo a0, Memo a1) => Memo (Either a0 a1) where
    data Table (Either a0 a1) b = TEither (Table a0 b) (Table a1 b)
    toTable f = TEither (toTable (f . Left)) (toTable (f . Right))
    fromTable (TEither r _) (Left a0) = fromTable r a0
    fromTable (TEither _ l) (Right  a1) = fromTable l a1

-- | Trie-like implementation
instance Memo a => Memo [a] where
    data Table [a] b = TList b (Table a (Table [a] b))
    toTable f = TList (f []) . toTable $ \x -> toTable $ \xs -> f (x:xs)
    fromTable (TList e _) [] = e
    fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs

-- | Memoize a function according to its instance of 'Memo'.
memoize :: Memo a => (a -> b) -> a -> b
memoize = fromTable . toTable

--------------------------------
-- Generic memoization functions
--------------------------------

-- | Memoize a function with a bounded domain into a boxed array.
asArray :: (Bounded a, Ix a) => (a -> b) -> a -> b
asArray f = memoize (f . getAsArray) . AsArray

-- | Memoize a function with a bounded domain into a tree according to its domain's enum instance.
asEnumTree :: (Bounded a, Enum a) => (a -> b) -> a -> b
asEnumTree f = memoize (f . getEnumTree) . AsEnumTree

-- | Memoize a function with a bounded domain into a tree according to its domain's integral instance.
asTree :: (Bounded a, Integral a) => (a -> b) -> a -> b
asTree f = memoize (f . getTree) . AsTree

------------------
-- Plain old array
------------------

newtype AsArray a = AsArray { getAsArray :: a }

instance (Bounded a, Ix a) => Memo (AsArray a) where
    data Table (AsArray a) b = ArrayT (Array a b)
    toTable f = ArrayT . listArray bounds . map (f . AsArray) $ range bounds
      where bounds = (minBound, maxBound)
    fromTable (ArrayT t) (AsArray a) = t ! a

------------------------------------
-- Intances using binary search tree
------------------------------------

newtype AsEnumTree a = AsEnumTree { getEnumTree :: a }

instance forall a. (Bounded a, Enum a) => Memo (AsEnumTree a) where
    data Table (AsEnumTree a) b = EnumTreeT (Tree Int b)
    toTable f = EnumTreeT $ mkTree (f . AsEnumTree . toEnum)
                                     (fromEnum (minBound :: a))
                                     (fromEnum (maxBound :: a))
    fromTable (EnumTreeT t) (AsEnumTree a) = unsafeLookup t $ fromEnum a

newtype AsTree a = AsTree { getTree :: a }

instance (Bounded a, Integral a) => Memo (AsTree a) where
    data Table (AsTree a) b = TreeT (Tree a b)
    toTable f = TreeT $ mkTree (f . AsTree) minBound maxBound
    fromTable (TreeT t) (AsTree a) = unsafeLookup t a

-----------------------------
-- Generic binary search tree
-----------------------------

-- An infinite binary tree
data Tree a b = Node a b (Tree a b) (Tree a b)

unsafeLookup :: Ord a => Tree a b -> a -> b
unsafeLookup t a = go t
  where
    go (Node x y l r) = case compare a x of LT -> go l
                                            GT -> go r
                                            EQ -> y

mkTree :: Integral a => (a -> b) -> a -> a -> Tree a b
mkTree f min max = go min max
  where
    go lo hi = if lo > hi
               then undefined
               else let mid = (lo + hi) `div` 2
                    in Node mid (f mid) (go (mid + 1) hi) (go lo (mid - 1))

