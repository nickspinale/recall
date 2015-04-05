{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Recall
    ( Memo
    , memoize
    ) where

import Data.Array

class Memo a where
    data Table a :: * -> *
    toTable :: (a -> b) -> Table a b
    fromTable :: Table a b -> a -> b

memoize :: Memo a => (a -> b) -> a -> b
memoize = fromTable . toTable

asArray :: (Bounded a, Ix a) => (a -> b) -> a -> b
asArray f = memoize (f . getAsArray) . AsArray

asEnumTree :: (Bounded a, Enum a) => (a -> b) -> a -> b
asEnumTree f = memoize (f . getAsEnumTree) . AsEnumTree

asTree :: (Bounded a, Integral a) => (a -> b) -> a -> b
asTree f = memoize (f . getAsTree) . AsTree

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

-- Trie-like
instance Memo a => Memo [a] where
    data Table [a] b = TList b (Table a (Table [a] b))
    toTable f = TList (f []) . toTable $ \x -> toTable $ \xs -> f (x:xs)
    fromTable (TList e _) [] = e
    fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs

------------------
-- Plain old array
------------------

newtype AsArray a = AsArray { getAsArray :: a }

instance (Bounded a, Ix a) => Memo (AsArray a) where
    data Table (AsArray a) b = TAsArray (Array a b)
    toTable f = TAsArray . listArray bounds . map (f . AsArray) $ range bounds
      where bounds = (minBound, maxBound)
    fromTable (TAsArray t) (AsArray a) = t ! a

------------------------------------
-- Intances using binary search tree
------------------------------------

newtype AsEnumTree a = AsEnumTree { getAsEnumTree :: a }

instance forall a. (Bounded a, Enum a) => Memo (AsEnumTree a) where
    data Table (AsEnumTree a) b = TAsEnumTree (Tree Int b)
    toTable f = TAsEnumTree $ mkTree (f . AsEnumTree . toEnum)
                                     (fromEnum (minBound :: a))
                                     (fromEnum (maxBound :: a))
    fromTable (TAsEnumTree t) (AsEnumTree a) = unsafeLookup t $ fromEnum a

newtype AsTree a = AsTree { getAsTree :: a }

instance (Bounded a, Integral a) => Memo (AsTree a) where
    data Table (AsTree a) b = TAsTree (Tree a b)
    toTable f = TAsTree $ mkTree (f . AsTree) minBound maxBound
    fromTable (TAsTree t) (AsTree a) = unsafeLookup t a

-----------------------------
-- Generic binary search tree
-----------------------------

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

