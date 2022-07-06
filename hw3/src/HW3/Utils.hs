{-# LANGUAGE LambdaCase #-}

module HW3.Utils where

import Data.Ratio

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
    xs@(_:_) -> take n xs : chunksOf n (drop n xs)
    []       -> []

ratioToIntegral :: (Integral r, Num i) => Ratio r -> Maybe i
ratioToIntegral r = case quotRem (numerator r) (denominator r) of
    (i, 0) -> Just $ fromIntegral i
    _      -> Nothing

addChar :: Char -> String -> String
addChar c [a] = [c] ++ [a]
addChar c  s  = s

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
