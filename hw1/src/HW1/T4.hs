module HW1.T4
  ( tfoldr 
  ) where

import HW1.T3

-- | Foldr a tree
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr = foldr

