{-# LANGUAGE LambdaCase #-}

module HW2.T5
  ( -- * Datatypes
    EvaluationError (..)
  , ExceptState (..)
    -- * map functions
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , wrapExceptState
  , throwExceptState
  ) where

import Control.Monad (ap, when)

import HW2.T1 
import HW2.T2 
import HW2.T4 (Expr(..), Prim(..), runS)
import qualified HW2.T4 

-- | ExceptT e State s a

data ExceptState e s a = ES
  { runES :: s -> Except e (Annotated s a)
  }

-- | fmap ExceptT e State s a

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \s -> mapExcept (mapAnnotated f) (runES es s)

-- | pure ExceptT e State s a

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> wrapExcept (a :# s)

-- | join ExceptT e State s a

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es = ES $ \s -> case runES es s of
    Error e             -> Error e
    Success (es' :# s') -> runES es' s'

-- modify ExceptT e State s a

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

-- | throwError ExceptT e State s a

throwExceptState :: e -> ExceptState e s a
throwExceptState e =  ES $ \_ -> Error e

instance Functor (ExceptState e s) where
    fmap = mapExceptState

instance Applicative (ExceptState e s) where
    pure  = wrapExceptState
    (<*>) = ap

instance Monad (ExceptState e s) where
    return   = pure
    m >>= k  = joinExceptState (fmap k m)


data EvaluationError = DivideByZero

-- | Evaluating an expression with both state and error throwing

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval = \case
    Val d -> pure d
    Op  e -> evalExp e
  where
    evalExp e@(Div a b) = do
        ea <- eval a
        eb <- eval b
        when (eb == 0) $ throwExceptState DivideByZero
        modifyExceptState (Div ea eb :)
        pure (ea / eb)
    evalExp e = ES $ \s -> Success $ runS (HW2.T4.evalExp e) s
