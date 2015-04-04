{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype BoundedIx a = BoundedIx { getBoundedIx :: a }
    deriving (Eq, Ord, Bounded, Ix)

instance (Bounded a, Ix a) => Memo (BoundedIx a) where
    data Table (BoundedIx a) b = TArray (Array (BoundedIx a) b)
    toTable f = TArray . listArray bounds . map f $ range bounds
      where bounds = (minBound, maxBound)
    fromTable (TArray t) a = t ! a

----------------------
-- Binary search trees
----------------------

-- class Scale a where
--     avg :: a

newtype BoundedEnum a = BoundedEnum { getBoundedEnum :: a }
    deriving (Eq, Ord, Bounded, Enum)

instance (Bounded a, Enum a) => Memo (BoundedEnum a) where
    data Table (BoundedIntegral a) b = Leaf | Branch Int b (Table Int b) (Table Int b)
    toTable f = go (fromEnum minBound) (fromEnum maxBound)
      where go lo hi = if lo > hi
                       then Leaf
                       else let mid = (lo + hi) `div` 2
                            in Branch mid (f $ fromEnum mid) (go lo (mid - 1)) (go (mid + 1) hi)
    fromTable t a = go t
      where
        e = fromEnum a
        go (Branch x y l r) | e == x = y
                            | e > x = go l
                            | otherwise = go r
        go Leaf = error "wat."


newtype BoundedIntegral a = BoundedIntegral { getBoundedIntegral :: a }
    deriving (Eq, Ord, Bounded, Num, Integral, Real, Enum)

instance (Bounded a, Integral a) => Memo (BoundedIntegral a) where
    data Table (BoundedIntegral a) b = Leaf
                                     | Branch (BoundedIntegral a)
                                              b
                                              (Table (BoundedIntegral a) b)
                                              (Table (BoundedIntegral a) b)
    toTable f = go minBound maxBound
      where go lo hi = if lo > hi
                       then Leaf
                       else let mid = (lo + hi) `div` 2
                            in Branch mid (f mid) (go lo (mid - 1)) (go (mid + 1) hi)
    fromTable t a = go t
      where
        go (Branch x y l r) | a == x = y
                            | a > x = go l
                            | otherwise = go r
        go Leaf = error "wat."
