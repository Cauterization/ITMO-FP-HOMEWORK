module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some))

-- | join

joinOption :: Option (Option a) -> Option a
joinOption (None)  = None
joinOption (Some (None)) = None
joinOption (Some (Some a )) = Some a 


joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success (Error e)) = Error e
joinExcept (Success (Success a )) = Success a



joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# b) :# c)= a :# (c <> b) 

myJoin :: (List a) -> (List a) -> (List a)
myJoin Nil b = b 
myJoin (a :. c) b =  let 
                        res =  myJoin c b 
                      in (a :. res)

joinList :: List (List a) -> List a
joinList  (Nil) = Nil 
joinList (a :. b) = myJoin a  (joinList b)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F a) = let revealFun (F a) i = a i
  in F $ \i -> revealFun (a i) i 
