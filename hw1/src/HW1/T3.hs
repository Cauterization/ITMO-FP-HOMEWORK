{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module HW1.T3
  ( -- | Write what does these functions do
    Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList
  ) where

import Data.Foldable
import Data.Function
import Data.List

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a) deriving (Show)

instance Foldable Tree where
    foldr f z (Branch _ l a r) = foldr f (f a (foldr f z r)) l
    foldr _ z _  = z

-- | size and depth of the tree
data Meta = M
    { s :: Int 
    , d :: Int 
    } deriving Show

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize     = \case
    Leaf -> 0
    Branch M{..} _ _ _ -> s
    
-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth = \case
    Leaf -> 0
    (Branch M{..} _ _ _) -> d

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember a = \case
    Leaf -> False
    Branch _ l b r -> if
        | a == b    -> True
        | a >  b    -> tmember a l
        | otherwise -> False

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a 
tinsert a = \case
    Leaf -> Branch (M 1 1) Leaf a Leaf 
    br@(Branch M{..} l b r) -> if
        | a == b    -> br
        | a <  b    -> let l' = rearange $ tinsert a l 
                       in rearange $ Branch (M (s + 1) (1 + max d (tdepth l'))) l' b r 
        | otherwise -> let r' = rearange $ tinsert a r
                       in rearange $ Branch (M (s + 1) (1 + max d (tdepth r'))) l b r' 

-- | Check if tree is balanced and rebild it if not
rearange :: Ord a => Tree a -> Tree a
rearange = \case
    Leaf -> Leaf
    br@(Branch _ l _ r) -> let valid f = (min `on` f) l r * 3 < (max `on` f) l r
                           in if valid tsize && valid tdepth 
                              then br
                              else tFromList $ toList br

-- | Build tree from list
tFromList :: Ord a => [a] -> Tree a
tFromList = go . sort
  where
    go = \case
        []  -> Leaf
        [a] -> Branch (M 1 1) Leaf a Leaf
        [a,b] -> Branch (M 2 2) (tFromList [a]) b Leaf
        [a,b,c] -> Branch (M 3 2) (tFromList [a]) b (tFromList [c])
        xs -> let len = length xs
                  (xl, xs') = splitAt (len `div` 2) xs
                  ([a], xr) = splitAt 1 xs'
                  l = tFromList xl
                  r = tFromList xr
              in Branch (M (tsize l + tsize r + 1) (1 + (max `on` tdepth) l r)) l a r 

