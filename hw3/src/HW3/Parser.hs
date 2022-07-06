{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module HW3.Parser
  ( parse
  ) where

import Data.Foldable
import Control.Applicative ((<|>))
import Control.Arrow
import Control.Monad.Combinators.Expr
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Function
import Data.String 
import Data.Char
import Data.Functor
import Data.Void (Void)
import Data.Map qualified as M
import Data.List
import Data.Maybe
import Data.Sequence as Seq
import Data.Scientific as Scientific
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char ( alphaNumChar, char, string, controlChar, space )
import Text.Megaparsec.Char.Lexer qualified as L
import HW3.Base

-- | Note that Parsec has IsString istance so with overloaded string
-- | we can write "abc" instead of string "abc"
type Parser = Parsec Void String

-- | Parser that skips arbitrary number of spaces

skipSpaces :: Parser String
skipSpaces = many $ satisfy isSpace

-- | Unary parser that skip spaces around argument
p_ :: Parser a -> Parser a
p_ a = skipSpaces *> a <* skipSpaces

-- | Parser operators that discards right or left 
-- | value (similar to <*, *>) and skip spaces in between

(</) ::  Parser a -> Parser b -> Parser a
(</) a b = a <* skipSpaces <* b
infixl 5 </

(/>) :: Parser a -> Parser b -> Parser b
(/>) a b = a *> skipSpaces *> b
infixl 5 />

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pExpr <* eof) "" 

-- | Parses exp1 `op` exp2
pExpr :: Parser HiExpr
pExpr = try (makeExprParser (p_ $ try pTerm <|> ("(" /> pExpr </ ")")) opTable) 
  where
    opTable = [ [ f InfixL "*"  HiFunMul
                , f InfixL "/"  HiFunDiv 
                ]           
              , [ f InfixL "+"  HiFunAdd
                , f InfixL "-"  HiFunSub 
                ]
              , [ f InfixN "<=" HiFunNotGreaterThan
                , f InfixN ">=" HiFunNotLessThan
                , f InfixN "==" HiFunEquals
                , f InfixN "/=" HiFunNotEquals
                , f InfixN "<"  HiFunLessThan
                , f InfixN ">"  HiFunGreaterThan 
                ]
              , [ f InfixR "&&" HiFunAnd 
                ]
              , [ f InfixR "||" HiFunOr 
              ] ]
    f assoc name func = 
        -- not followed by is here to avoid parsing errors with "==" and "=" etc.
        let p = try $ string name <* notFollowedBy "="
        in assoc $ (\a b -> HiExprApply (HiExprValue $ HiValueFunction func) [a,b]) <$ p

-- | Parses val `ap` fun `ap` fun ...
pTerm :: Parser HiExpr
pTerm = foldl (&) <$> val <*> ap
    where val = choice 
            [ "(" /> pExpr </ ")"
            , HiExprValue <$> pValue
            , pList 
            , pDict 
            ]
          ap = many $ try $ choice
            [ HiExprRun <$ notFollowedBy " " <*  "!" <* skipSpaces
            , flip HiExprApply <$> (notFollowedBy " " *> pDotStrings)
            , try $ p_ $ flip HiExprApply <$> "(" /> pEnum pExpr </ ")"
            ]

-- | Parses plist = [ exp1, exp2, .. ] or (plist)
pList :: Parser HiExpr
pList =  HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> "[" /> pEnum pExpr </ "]"
    <|> "(" /> pList </ ")"

-- | Parses pdict = { expK1: expV1, expK2: expV2, ..  } or (pdict)
pDict :: Parser HiExpr
pDict = HiExprDict <$> "{" /> pEnum ((,) <$> (p_ $ pExpr </ ": ") <*> (p_ $ pExpr)) </ "}"
    <|> "(" /> pDict </ ")"

-- | Parses strings that starts with .
pDotStrings :: Parser [HiExpr]
pDotStrings = do
    _  <- "."
    x  <- satisfy isAlpha
    xs <- many $ satisfy isAlphaNum <|> char '-' <* notFollowedBy (char '-') 
    return $ pure $ HiExprValue $ HiValueString $ T.pack $ x : xs

-- | Parses p, p, ... p
pEnum :: Parser a -> Parser [a]
pEnum p = (<|> pure []) $ do
        es <- many $ try $ try p </ ","
        e <- p_ p
        return $ es <> [e]


pValue :: Parser HiValue
pValue = choice 
    [ try $ "(" /> pValue </ ")" 
    , pNum    <&> HiValueNumber    
    , pFun    <&> HiValueFunction 
    , pAction <&> HiValueAction    
    , (True <$ "true" <|> False <$ "false") <&> HiValueBool  
    , "null"   $> HiValueNull
    , pString <&> HiValueString     
    , pBytes  <&> HiValueBytes  
    ]

pNum :: Parser Rational
pNum = let f = toRational <$> L.scientific 
       in  "-" /> (negate <$> f) <|> f

pFun :: Parser HiFun
pFun = choice 
    [ HiFunDiv            <$ ("/" <* notFollowedBy "=" <|> "div")
    , HiFunMul            <$ ("*"  <|> "mul")
    , HiFunAdd            <$ ("+"  <|> "add")
    , HiFunSub            <$ ("-"  <|> "sub")
    , HiFunNotLessThan    <$ (">=" <|> "not-less-than")
    , HiFunNotGreaterThan <$ ("<=" <|> "not-greater-than")
    , HiFunNotEquals      <$ ("/=" <|> "not-equals")
    , HiFunIf             <$ "if"
    , HiFunNot            <$ ("not" <* notFollowedBy "-")
    , HiFunAnd            <$ ("&&" <|> "and")
    , HiFunOr             <$ ("||" <|> "or" )
    , HiFunLessThan       <$ ("<" <* notFollowedBy "=" <|> "less-than")
    , HiFunGreaterThan    <$ (">" <* notFollowedBy "=" <|> "greater-than")
    , HiFunEquals         <$ ("==" <|> "equals")
    , HiFunLength         <$ "length"            
    , HiFunToUpper        <$ "to-upper"   
    , HiFunToLower        <$ "to-lower"   
    , HiFunReverse        <$ "reverse"   
    , HiFunTrim           <$ "trim"
    , HiFunList           <$ "list"
    , HiFunRange          <$ "range"
    , HiFunFold           <$ "fold"
    , HiFunPackBytes      <$ "pack-bytes"
    , HiFunUnpackBytes    <$ "unpack-bytes"  
    , HiFunEncodeUtf8     <$ "encode-utf8"  
    , HiFunDecodeUtf8     <$ "decode-utf8"  
    , HiFunZip            <$ "zip"   
    , HiFunUnzip          <$ "unzip"    
    , HiFunSerialise      <$ "serialise"  
    , HiFunDeserialise    <$ "deserialise"
    , HiFunRead           <$ "read"
    , HiFunWrite          <$ "write"
    , HiFunMkDir          <$ "mkdir"
    , HiFunChDir          <$ "cd"
    , HiFunParseTime      <$ "parse-time"
    , HiFunRand           <$ "rand"
    , HiFunEcho           <$ "echo"
    , HiFunCount          <$ "count"    
    , HiFunKeys           <$ "keys"
    , HiFunValues         <$ "values"
    , HiFunInvert         <$ "invert"  
    ]

-- | Other actions parses as expression aplyes
pAction :: Parser HiAction
pAction = choice 
    [ HiActionCwd <$ "cwd"
    , HiActionNow <$ "now"
    ]

pString :: Parser Text
pString = do 
    _  <- char '"'
    xs <- try $ many $ try ("\n" <$ "\\n") 
                   <|> try ("\"" <$ "\\\"") 
                   <|> try ("\\" <$ "\\\\") 
                   <|> pure <$> satisfy (\x -> isSpace x || x /= '"')
    _  <- char '"'           
    return $ T.pack $ concat xs 
    
pBytes :: Parser ByteString
pBytes = "[#" /> (try argsB <|> pure "") </ "#]"
  where 
    argsB = do
        b  <- hex
        bs <- many $ try $ " " *> hex
        return $ B.concat $ b : bs
    hex = let h = fromString . pure <$> satisfy isHexDigit 
          in (<>) <$> h <*> h
