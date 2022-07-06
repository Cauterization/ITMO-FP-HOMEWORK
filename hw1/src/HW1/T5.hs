module HW1.T5 where

import Data.List.NonEmpty (NonEmpty ((:|)), (<|))

-- | Split a non-empty list by a separator
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep []      = [] :| [] 
splitOn sep [x]     = if x == sep then [] :| [[]]  else [x] :| [] 
splitOn sep (x:xs)  = let y :| ys = splitOn sep xs in
                      if x == sep then [] :| y : ys else (x : y) :| ys

-- | Dual to splitOn           
joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep ([] :| [])      = []
joinWith sep (x  :| [])      = x
joinWith sep ([] :| (x:xs))  = sep : joinWith sep (x :| xs)
joinWith sep (x1 :| (x2:xs)) = x1 <> (sep : joinWith sep (x2 :| xs))
