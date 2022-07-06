{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module HW2.T6
  {-( -- * Datatypes
    ParseError (..)
  , Parser (..)
    -- * functions
  , pChar
  , pEof
  , parseError
  , parseExpr
  , runP
  )-} where

import Control.Applicative 
import Control.Monad 
import Data.Foldable
import Data.Char
import Data.Function


import GHC.Natural (Natural)
import HW2.T1 (Annotated ((:#)), Except (..), mapExcept)
import HW2.T4 (Expr (Op, Val), Prim (Add, Div, Mul, Sub))
import HW2.T5 (ExceptState (ES, runES), throwExceptState)


newtype ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
   deriving newtype (Functor, Applicative, Monad)

-- | Parser runner

runP :: Parser a -> String -> Except ParseError a
runP (P p) s = mapExcept (\(a :# _) -> a) $ runES p (0, s) 

-- | Char parser

pChar :: Parser Char
pChar = P $ ES \(pos, s) -> case s of
    -- throws error with remembered position when string is empty
    []     -> Error (ErrorAtPos pos)    
    -- inc position, take head from string and return it
    (c:cs) -> Success (c :# (pos + 1, cs))

-- | Parser that always fails

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error $ ErrorAtPos pos


instance Alternative Parser where
   empty = parseError
   (P a) <|> (P b) = P $ ES \(pos, s) -> case runES a (pos, s) of
       Success a -> Success a
       _         -> runES b (pos, s)

instance MonadPlus Parser   -- No methods.

instance MonadFail Parser where
    fail _ = empty

-- | EOF parser

pEof :: Parser ()
pEof = P $ ES \(pos, s) -> if null s then Success (() :# (pos, s)) else Error $ ErrorAtPos pos

-- | Expression parser

parseExpr :: String -> Except ParseError Expr
parseExpr s = flip runP s $ do
    res <- pExpr
    _   <- pEof
    pure res

type Precedence = Int

-- Expressions are first parsed into the following expression tokens 
data Token e = TExpr e
             | TAp   Precedence (e -> e -> Prim e) e

{- So, all correct expressions can be represented as [A, (`op1` B), (`op2` C) .. (`opN` N)]
where A ~ TExpr is a simple value or other expression in parrens, and (`op` X) ~ TAp 
is operator applied to other value or expression in parrens
-}

pExpr :: Parser Expr
pExpr = do
    _   <- space
    val <- TExpr <$> (parens pExpr <|> pVal)
    aps <- many $ space >> pExprAp
    x   <- foldExpr 2 $ val : aps
    _   <- space
    pure x

  where

{- folding list with helper func applyExp 2 times - for one
to each possible precedence. 
-}
    foldExpr :: Precedence -> [Token Expr] -> Parser Expr
    foldExpr 0 [TExpr x]   = pure x
    foldExpr p (val : aps) = applyExp p val aps >>= foldExpr (p - 1)
    foldExpr _ x           = fail ""
    
{- if TAp has the desired precedence
, then it is applied to the accamulated expression
otherwise it will be applied in next foldExpr
with a lower precedence check.
-}                            -- Acc        -- Remaining unprocessed values
    applyExp :: Precedence -> Token Expr -> [Token Expr]         -> Parser [Token Expr]
    applyExp    _             e             []                    = pure [e]
    applyExp    p             (TExpr e)     (TAp ap af ae : as)
        | ap == p                                                 = applyExp p (TExpr (Op (af e ae))) as
        | otherwise                                               = ((TAp ap af e) :) <$> (applyExp p (TExpr ae) as)
    applyExp    p             (TAp ap af ae) (TAp bp bf be : bs)  = applyExp p (TAp bp bf (Op (af ae be))) bs
    applyExp    p             (TAp ap af ae) [TExpr e]            = pure [TExpr (Op $ af ae e)]
    applyExp    _             _              _ = fail ""

-- | (ExpA | ValA) `op` (ValB | ExpB) parser

pExprAp :: Parser (Token Expr)
pExprAp = do
    op  <- prioritized 1 '+' Add 
       <|> prioritized 1 '-' Sub
       <|> prioritized 2 '*' Mul
       <|> prioritized 2 '/' Div
    _   <- space
    val <- pVal <|> parens pExpr 
    pure $ op val
  where
    prioritized p c f = TAp p f <$ char c

-- | ValA parser

pVal :: Parser Expr
pVal =  do
    i <- some (satisfy isDigit)
    f <- ((:) <$> char '.' <*> some (satisfy isDigit)) <|> pure ""
    pure $ Val $ read $ i <> f

-- | Get rest of the string

getString :: Parser String
getString = P $ ES \(pos, s) -> Success (s :# (pos, s))

-- | Put new string

putString :: String -> Parser ()
putString s = P $ ES \(pos, _) -> Success (() :# (pos, s))

-- | Take a head from the string

decString :: Parser Char
decString = P $ ES \(pos, s) -> case s of
    c:cs -> Success (c :# (pos + 1, cs))
    _    -> Error $ ErrorAtPos pos

-- | Parser between parentheses and an arbitrary number of spaces

parens :: Parser a -> Parser a
parens p = do
    _ <- space
    _ <- char '('
    _ <- space
    x <- p
    _ <- space
    _ <- char ')'
    _ <- space
    pure x

-- | Parser with a predicate

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    c <- decString
    guard (p c) 
    pure c

-- | Any char

char :: Char -> Parser Char
char = satisfy . (==)

-- | Arbitrary number of spaces

space :: Parser String
space = many $ char ' '
