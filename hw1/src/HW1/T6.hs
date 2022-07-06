{-# LANGUAGE TupleSections #-}

module HW1.T6 where

import Data.Foldable 

-- | monoid composition concat
mcat :: Monoid a => [Maybe a] -> a
mcat = fold . fold 

-- | monoidal bifunctor concat?
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap (either (,mempty) (mempty,))