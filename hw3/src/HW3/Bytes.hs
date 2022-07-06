{-# LANGUAGE ImportQualifiedPost #-}

module HW3.Bytes where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char 
import Numeric

import HW3.Utils 

numsToBytes :: B.ByteString -> Maybe B.ByteString
numsToBytes = fmap BC.pack . ntb . BC.unpack
  where
    ntb nums
        | even (length nums) && all isHexDigit nums 
            = Just $ map (chr . f) $ chunksOf 2 $ nums
        | otherwise = Nothing 

bytesToNums :: B.ByteString -> B.ByteString
bytesToNums = BC.pack .  concatMap (addChar '0' . (flip showHex) "" . ord)  . BC.unpack

bytesToHexes :: B.ByteString -> [Int]
bytesToHexes = map f . chunksOf 2 . BC.unpack . bytesToNums

f :: String -> Int
f = fst . head . readHex