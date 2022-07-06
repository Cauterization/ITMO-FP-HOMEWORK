module HW2.T2
  ( -- * dist functions
    distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
    -- * wrap functions
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some), Pair (P), Prioritised (High, Low, Medium), Quad (Q),
               Stream ((:>)))
import Prelude (Monoid, Semigroup, mempty, (<>), undefined, ($))
import Data.List (tail)
import Data.List

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some a, Some b) = Some (a, b)

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair ((P a b), (P c d)) = P (a, c) (b, d)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad ((Q a1 b1 c1 d1), (Q a2 b2 c2 d2)) = Q (a1, a2) (b1, b2) (c1, c2) (d1, d2) 

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated ((a :# b) , (c :# d)) = (a, c) :# (b <> d)


distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success a, Success b) = Success (a, b)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b)  = High (a, b)
distPrioritised (High a, Medium b) = High (a, b)
distPrioritised (High a, Low b) = High (a, b)
distPrioritised (Medium a, High b) = High (a, b)
distPrioritised (Low a, High b) = High (a, b)
distPrioritised (Medium a, Medium b)  = Medium (a, b)
distPrioritised (Low a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b) = Medium (a, b)
distPrioritised (Low a, Low b ) = Low (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (((a :>  b), ((c :>  d))))= (a, c) :> (distStream (b,d))



distListLeft :: (a, List b, List (a, b)) -> List (a,b)
distListLeft (_ , Nil, x) = x
distListLeft (a, c :. d, x) = distListLeft(a, d, (a, c) :. x)

myJoin :: List(a, b) -> List (a, b) -> List (a, b)
myJoin Nil b = b 
myJoin (a :. b) c =  myJoin b (a :. c)  

distList :: (List a, List b) -> List (a, b)
distList (_, Nil) = Nil 
distList (Nil, _) = Nil 
distList (a :. b, c :. d) =  myJoin (distListLeft(a, c :. d, Nil))   (distList (b, c :. d))

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F a, F b) = F $ (\i -> (a i, b i))

-- | pure

wrapOption :: a -> Option a
wrapOption a = Some a 

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a =  Q a a a a 

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty 

wrapExcept :: a -> Except e a
wrapExcept a = Success a 

wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = Low a   

wrapStream :: a -> Stream a
wrapStream a = a :> (wrapStream a)

wrapList :: a -> List a
wrapList a =  (:. Nil) a

wrapFun :: a -> Fun i a
wrapFun a = F $ \i -> a 
