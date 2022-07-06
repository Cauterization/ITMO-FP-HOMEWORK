{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module HW3.Pretty
  ( prettyValue
  , prettyErr
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Ratio
import           Data.Foldable
import           Data.List
import           Data.Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as Time
import qualified Data.Map as M
import           Numeric
          
import           Prettyprinter 
import           Prettyprinter.Render.Terminal 
          
import           HW3.Base
import           HW3.Bytes
import           HW3.Utils
import Language.Haskell.TH (Extension(ScopedTypeVariables))

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = renderValue

prettyErr :: HiError -> Doc AnsiStyle
prettyErr =  pretty . show

renderValue :: HiValue -> Doc a
renderValue = \case        
    HiValueBool True  -> "true"
    HiValueBool False -> "false"
            
    HiValueNumber n   -> renderNum n
                                        
    HiValueFunction f -> renderFunc f
            
    HiValueNull       -> "null"
            
    HiValueString t   -> pretty $ T.concat ["\"", t, "\""]

    HiValueList lst   -> case toList lst of
        [] -> "[ ]"
        l  -> mconcat ["[ ", mconcat $ intercalate [", "] (map (pure . renderValue) l), " ]"] 
                             
    HiValueAction a -> renderAction a
                    
    HiValueBytes "" -> "[# #]"
    HiValueBytes bs -> mconcat ["[# ", renderBytes bs, " #]"]
                    
    HiValueTime t   -> mconcat ["parse-time(\"", pretty $ show t, "\")"]
                    
    HiValueDict d   -> case M.toList d of
        []     -> "{ }"
        (x:xs) -> mconcat ["{ ", foldl (\ini y -> ini <> ", " <> renderPair y) (renderPair x) $ xs, " }"]

renderNum :: Rational -> Doc a
renderNum x = case fromRationalRepetendUnlimited x of
    (n, Nothing) -> case floatingOrInteger n of
        Right (i :: Integer) -> pretty i
        Left f -> pretty $ showFFloat Nothing f ""
    _            -> renderRatio x

renderRatio :: Rational -> Doc a
renderRatio n = pretty . concat $ if
    | simple    -> [show a,  "/", show b]
    | positive  -> [show x, " + ", show y, "/", show $ abs b] 
    | otherwise -> [show x, " - ", show $ abs y, "/", show $ abs b]
  where
    simple = abs a < abs b
    positive = a > 0
    a     = numerator n
    b     = denominator n
    (x,y) = quotRem a b  

renderExpr :: HiExpr -> Doc ann
renderExpr (HiExprValue v) = renderValue v
renderExpr e = pretty $ show e

renderPair :: (HiValue, HiValue) -> Doc a
renderPair (a,b) = renderValue a <> ": " <> renderValue b 

renderFunc :: HiFun -> Doc a
renderFunc = \case
    HiFunDiv            -> "div"       
    HiFunMul            -> "mul"       
    HiFunAdd            -> "add"       
    HiFunSub            -> "sub"   
    HiFunNot            -> "not"      
    HiFunAnd            -> "and"       
    HiFunOr             -> "or"   
    HiFunLessThan       -> "less-than"           
    HiFunGreaterThan    -> "greater-than"               
    HiFunEquals         -> "equals"         
    HiFunNotLessThan    -> "not-less-than"               
    HiFunNotGreaterThan -> "not-greater-than"                  
    HiFunNotEquals      -> "not-equals"             
    HiFunIf             -> "if"     
    HiFunLength         -> "length"         
    HiFunToUpper        -> "to-upper"          
    HiFunToLower        -> "to-lower"          
    HiFunReverse        -> "reverse"          
    HiFunTrim           -> "trim"      
    HiFunList           -> "list"
    HiFunRange          -> "range"
    HiFunFold           -> "fold"
    HiFunPackBytes      -> "pack-bytes"      
    HiFunUnpackBytes    -> "unpack-bytes"       
    HiFunEncodeUtf8     -> "encode-utf8"     
    HiFunDecodeUtf8     -> "decode-utf8"     
    HiFunZip            -> "zip"    
    HiFunUnzip          -> "unzip"  
    HiFunSerialise      -> "serialise"   
    HiFunDeserialise    -> "deserialise"
    HiFunRead           -> "read"  
    HiFunWrite          -> "write"   
    HiFunMkDir          -> "mkdir"   
    HiFunChDir          -> "cd"   
    HiFunParseTime      -> "parse-time"
    HiFunRand           -> "rand"
    HiFunEcho           -> "echo"
    HiFunCount          -> "count"  
    HiFunKeys           -> "keys" 
    HiFunValues         -> "values"     
    HiFunInvert         -> "invert"    

renderAction :: HiAction -> Doc ann
renderAction a = mconcat $ case a of
    HiActionRead  fp    -> ["read",  "(\"", pretty fp, "\")"]
    HiActionWrite fp bs -> ["write", "(\"", pretty fp, "\", ", renderValue (HiValueBytes bs), ")"]
    HiActionMkDir fp    -> ["mkdir", "(\"", pretty fp, "\")"]
    HiActionChDir fp    -> ["cd", "(\"", pretty fp, "\")"]
    HiActionCwd         -> ["cwd"]
    HiActionNow         -> ["now"]
    HiActionRand  a b   -> ["rand(", pretty a, ", ", pretty b, ")"]
    HiActionEcho  t     -> ["echo", "(\"", pretty t, "\")"]      

renderBytes :: ByteString -> Doc ann
renderBytes = pretty . unwords . chunksOf 2 . B.unpack . bytesToNums






