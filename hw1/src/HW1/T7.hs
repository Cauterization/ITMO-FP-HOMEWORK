{-# LANGUAGE DerivingVia #-}

module HW1.T7 where

import Data.Monoid (Endo(..))

data ListPlus a = a :+ ListPlus a | Last a deriving Show
infixr 5 :+

instance Semigroup (ListPlus a) where
    Last a    <> b    = a :+ b
    (a :+ as) <> b    = a :+ (as <> b)

data Inclusive a b = This a | That b | Both a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
    This a1    <> This a2    = This $ a1 <> a2 
    This a     <> That b     = Both a b
    This a1    <> Both a2 b  = Both (a1 <> a2) b
    That b     <> This a     = Both a b
    That b1    <> That b2    = That $ b1 <> b2
    That b1    <> Both a b2  = Both a $ b1 <> b2
    Both a1 b  <> This a     = Both (a1 <> a) b
    Both a b1  <> That b2    = Both a $ b1 <> b2 
    Both a1 b1 <> Both a2 b2 = Both (a1 <> a2) (b1 <> b2)

newtype DotString = DS String deriving Show
    deriving Monoid via String

instance Semigroup DotString where
    DS "" <> DS b  = DS b
    DS a  <> DS "" = DS a
    DS a  <> DS b  = DS $ a <> "." <> b 

-- | F is Endomorhism
newtype Fun a = F (a -> a)
    deriving (Semigroup, Monoid) via (Endo a)