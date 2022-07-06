module HW1.T2
  ( N (..)
  , nFromNatural
  , nToNum
  , ncmp
  , nmult
  , nplus
  , nsub
    -- * Advanced
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import GHC.Natural (Natural)

data N =
    Z
  | S N

nplus :: N -> N -> N        -- addition
nplus Z     n = n
nplus (S m) n = S (nplus m n)

nmult :: N -> N -> N        -- multiplication
nmult Z     _ = Z
nmult (S m) n = n `nplus` (m `nmult` n)

npred :: N -> Maybe N
npred Z     = Nothing
npred (S n) = Just n

nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
nsub m Z = Just m
nsub m (S n) = nsub m n >>= npred

ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)
ncmp Z     Z     = EQ
ncmp Z     _     = LT
ncmp _     Z     = GT
ncmp (S n) (S m) = ncmp n m 

nFromNatural :: Natural -> N
nFromNatural n = (!! fromEnum n) $ iterate S Z

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + (nToNum n)

-- | Advanced

nEven, nOdd :: N -> Bool    -- parity checking
nOdd Z         = False
nOdd (S Z)     = True
nOdd (S (S n)) = nOdd n

nEven = not . nOdd

ndiv :: N -> N -> N         -- integer division
ndiv a b = maybe Z (\x -> S Z `nplus` (x `ndiv` b)) (a `nsub` b)

nmod :: N -> N -> N         -- modulo operation
nmod a b = case ncmp a b of
    LT -> a
    _  -> maybe Z (`nmod` b) (a `nsub` b) 
