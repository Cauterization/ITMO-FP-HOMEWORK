{-# LANGUAGE FlexibleInstances , LambdaCase, StandaloneDeriving  #-}

module HW2.T4
  ( -- * Datatypes
    Expr (..)
  , Prim (..)
  , State (..)
    -- * map functions
  , eval
  , evalExp
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import qualified Control.Monad
import HW2.T1 (Annotated ((:#)), mapAnnotated)
import Control.Monad (join)

import Data.Functor 
import Data.Ratio 

data State s a = S
  { runS :: s -> Annotated s a
  }

-- | map

mapState :: (a -> b) -> State s a -> State s b
mapState f s =  S $ mapAnnotated f . runS s 

-- | pure

wrapState :: a -> State s a
wrapState a = S $ (a :#) 

-- | join

joinState :: State s (State s a) -> State s a
joinState (S run) = S $ \s ->
  case run s of
    (S newRun) :# a -> newRun a 

-- | modify

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s 

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- ^ (+)
  | Sub a a      -- ^ (-)
  | Mul a a      -- ^ (*)
  | Div a a      -- ^ (/)
  | Abs a        -- ^ abs
  | Sgn a        -- ^ signum

data Expr =
  Val Double
  | Op (Prim Expr)

instance Num Expr where
  x + y         = Op  (Add x y)
  x - y         = Op  (Sub x y)
  x * y         = Op  (Mul x y)
  abs a         = Op  (Abs a)
  signum a      = Op  (Sgn a)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  fromRational r = Val $ fromRational r
  a / b          = Op (Div a b)

-- | Evaluating an expression with state

eval :: Expr -> State [Prim Double] Double
eval = \case
    Val d -> pure d
    Op  e -> evalExp e

evalExp :: Prim Expr -> State [Prim Double] Double
evalExp = \case
    Add a b -> (bin (+)    Add) a b
    Sub a b -> (bin (-)    Sub) a b
    Mul a b -> (bin (*)    Mul) a b
    Div a b -> (bin (/)    Div) a b
    Abs a   -> (un  abs    Abs) a 
    Sgn a   -> (un  signum Sgn) a 
bin op constr a b = do
    ea <- eval a
    eb <- eval b
    modifyState (constr ea eb :)
    pure (ea `op` eb)
un op constr a = do
    ea <- eval a
    modifyState (constr ea :)
    pure $ op ea



