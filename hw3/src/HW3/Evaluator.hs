{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HW3.Evaluator where


import Control.Monad.Except
import Control.Monad.Catch
import Codec.Compression.Zlib
import Codec.Serialise qualified as S
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe
import Data.Time qualified as Time
import Data.Tuple
import Data.Char 
import Data.Coerce 
import Data.Function
import Data.Semigroup qualified as Semigroup
import Data.List
import Data.Ratio 
import Data.Text (Text)
import Data.Text qualified as T 

import Data.Text.Encoding qualified as T
import Data.Sequence qualified as Seq

import Text.Read (readMaybe)

import HW3.Action 
import HW3.Base 
import HW3.Bytes 
import HW3.Utils 


-- | Evaluation :

eval :: (HiMonad m) => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . unEval . e 

-- | Wrapper for our evaluations
newtype EvalT m a = EvalT {unEval :: ExceptT HiError m a}
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadError HiError)

instance HiMonad m => HiMonad (EvalT m) where
    runAction a = lift $ runAction a

-- | Evaluating class

class Eval exp where
    e :: (HiMonad m) => exp -> EvalT m HiValue

-- | Expressions :

instance Eval HiExpr where
    e (HiExprValue (HiValueBytes bs)) = return $ maybe HiValueNull HiValueBytes $ numsToBytes bs 

    e (HiExprValue v)                 = return v

    e (HiExprApply exp args)          = e exp >>= \case
        HiValueFunction f -> evalFun f args      
        HiValueString s   -> traverse e args >>= evalHiList s
        HiValueList l     -> traverse e args >>= evalHiList l
        HiValueBytes b    -> traverse e args >>= evalHiList b
        HiValueDict d     -> traverse e args >>= evalHiDict d
        _ -> throwError $ HiErrorInvalidFunction

    e (HiExprRun exp)                 = do
        x <- e exp
        case x of
            HiValueAction a@(HiActionRand l u) -> if
                | l == u -> return $ HiValueNumber $ fromIntegral l
                | max (abs l) (abs u) > 1234567898765432122 -> throwError $ HiErrorInvalidArgument
                | otherwise -> runAction a
            HiValueAction a -> runAction a
            _ -> throwError $ HiErrorInvalidArgument

    e (HiExprDict dic) = HiValueDict . M.fromList <$> traverse evalPair dic

-- | Functions :

data Strict
data Lazy

data Unary
unary :: [HiFun]
unary = 
    [ HiFunNot
    , HiFunLength
    , HiFunToUpper
    , HiFunToLower
    , HiFunReverse
    , HiFunTrim
    , HiFunPackBytes
    , HiFunUnpackBytes
    , HiFunEncodeUtf8
    , HiFunDecodeUtf8
    , HiFunSerialise
    , HiFunDeserialise
    , HiFunZip
    , HiFunUnzip
    , HiFunRead
    , HiFunMkDir
    , HiFunChDir
    , HiFunParseTime
    , HiFunEcho
    , HiFunInvert
    ]

data Binary
binary :: [HiFun]
binary =
    [ HiFunDiv
    , HiFunMul
    , HiFunAdd
    , HiFunSub
    , HiFunAnd
    , HiFunOr
    , HiFunLessThan
    , HiFunGreaterThan
    , HiFunEquals
    , HiFunNotLessThan
    , HiFunNotGreaterThan
    , HiFunNotEquals
    , HiFunRange
    , HiFunWrite
    , HiFunMkDir
    , HiFunRand
    , HiFunFold
    ]

data Ternary
data CheckedArity
-- | s - strictness
-- | a - arity
-- | v - type of args (exprs or values)
data Fun s a v = Fun HiFun [v] deriving Show

-- | Typecast to CheckedArity arity
toCheckedArity :: Fun s a v -> Fun s CheckedArity v
toCheckedArity = coerce

-- | Typecast to Strict evaluation
toStrict :: Fun s a v -> Fun Strict a v
toStrict = coerce

-- | Function evaluating routing
evalFun :: (HiMonad m) => HiFun -> [HiExpr] -> EvalT m HiValue
evalFun f           args = let x = Fun f args in if
    | f == HiFunIf    ->   e @(Fun Lazy Ternary      HiExpr) x
    | f `elem` unary  ->   e @(Fun Lazy Unary        HiExpr) x
    | f `elem` binary ->   e @(Fun Lazy Binary       HiExpr) x
    | otherwise       ->   e @(Fun Lazy CheckedArity HiExpr) x

-- | Arity check
instance forall s e. Eval (Fun s CheckedArity e) => Eval (Fun s Unary e) where
    e f@(Fun _ [_]) = e $ toCheckedArity f
    e _             = throwError HiErrorArityMismatch

instance forall s e. Eval (Fun s CheckedArity e) => Eval (Fun s Binary e) where
    e f@(Fun _ [_,_]) = e $ toCheckedArity f
    e _               = throwError HiErrorArityMismatch

instance forall s e. Eval (Fun s CheckedArity e) => Eval (Fun s Ternary e) where
    e f@(Fun _ [_,_,_]) = e $ toCheckedArity f
    e _                 = throwError HiErrorArityMismatch

-- | Evaluating lazy function
instance ( Eval (Fun Strict CheckedArity HiExpr)) =>
           Eval (Fun Lazy CheckedArity HiExpr) where
    e (Fun HiFunIf [exp, l, r]) = do
        p <- e exp
        case p of
            HiValueBool True  -> e l
            HiValueBool False -> e r
            HiValueNull       -> e r
            _                 -> throwError HiErrorInvalidArgument

    e (Fun HiFunAnd [a, b]) = do
        l <- e a
        case l of
            (HiValueBool False) -> return $ HiValueBool False
            HiValueNull         -> return $ HiValueNull
            _                   -> e b

    e (Fun HiFunOr [a, b]) = do
        l <- e a
        case l of
            (HiValueBool False) -> e b
            HiValueNull         -> e b
            a                   -> return a

    e f = e $ toStrict f

-- | Evaluating strict function with unevaluated args 
instance Eval (Fun Strict CheckedArity HiExpr) where
    e (Fun f args) = do
        args' <- traverse e args
        e @(Fun Strict CheckedArity HiValue) $ Fun f args'

-- | Evaluating strict function with evaluated args
instance Eval (Fun Strict CheckedArity HiValue) where
    e (Fun HiFunDiv [a,b]) = case (a,b) of
        (HiValueNumber a, HiValueNumber 0) -> throwError $ HiErrorDivideByZero
        (HiValueNumber a, HiValueNumber b) -> return $ HiValueNumber $ a / b   
        (HiValueString a, HiValueString b) -> return $ HiValueString $ a <> "/" <> b
        _                                  -> throwError HiErrorInvalidArgument 

    e (Fun HiFunMul [a,b]) = case (a,b) of
        (HiValueNumber  a, HiValueNumber times) -> return $ HiValueNumber $ a * times
        (HiValueString ss, HiValueNumber times) -> sTimes ss times
        (HiValueList   xs, HiValueNumber times) -> sTimes xs times
        (HiValueBytes  bs, HiValueNumber times) -> sTimes bs times
        _                                       -> throwError HiErrorInvalidArgument 

    e (Fun HiFunAdd [a,b]) = case (a,b) of
        (HiValueNumber a, HiValueNumber b) -> return $ HiValueNumber $ a + b
        (HiValueString a, HiValueString b) -> append a b 
        (HiValueList   a, HiValueList   b) -> append a b 
        (HiValueBytes  a, HiValueBytes  b) -> append a b 
        (HiValueTime   a, HiValueNumber b) -> return $ HiValueTime $ Time.addUTCTime (realToFrac b) a
        _                                  -> throwError HiErrorInvalidArgument 

    e (Fun HiFunSub [a,b]) = case (a,b) of
        (HiValueTime   a, HiValueTime   b) -> return $ HiValueNumber $ toRational $ Time.diffUTCTime a b
        (HiValueNumber a, HiValueNumber b) -> return $ HiValueNumber $ a - b
        _                                  -> throwError HiErrorInvalidArgument 

    e (Fun HiFunNot            [a])    = HiValueBool <$> hiNot a
          
    e (Fun HiFunLessThan       [a, b]) = HiValueBool <$> hiLt (a, b)
      
    e (Fun HiFunEquals         [a, b]) = HiValueBool <$> hiEq (a, b)
   
    e (Fun HiFunGreaterThan    [a, b]) = HiValueBool . not <$> ((||) <$> hiEq (a, b) <*> hiLt (a,b))
   
    e (Fun HiFunNotEquals      [a, b]) = HiValueBool . not <$> hiEq (a, b)
   
    e (Fun HiFunNotLessThan    [a, b]) = HiValueBool . not <$> hiLt (a, b)

    e (Fun HiFunNotGreaterThan [a, b]) = HiValueBool <$> ((||) <$> hiLt (a, b) <*> hiEq (a, b))

    e (Fun HiFunLength [a]) = case a of
        HiValueString s -> return $ HiValueNumber $ fromIntegral $ T.length  s
        HiValueList  xs -> return $ HiValueNumber $ fromIntegral $   length xs
        HiValueBytes bs -> return $ HiValueNumber $ fromIntegral $ B.length bs
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunToUpper [s]) = case s of
        HiValueString s -> return $ HiValueString $ T.map toUpper $ s
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunToLower [s]) = case s of
        HiValueString s -> return $ HiValueString $ T.map toLower $ s
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunReverse [a]) = case a of
        HiValueString s -> return $ HiValueString $   T.reverse  s
        HiValueList  xs -> return $ HiValueList   $ Seq.reverse xs
        HiValueBytes bs -> return $ HiValueBytes  $   B.reverse bs
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunTrim [a]) = case a of 
        HiValueString s -> return $ HiValueString $ T.pack $ trim $ T.unpack s
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunList xs) = return $ HiValueList $ Seq.fromList xs 

    e (Fun HiFunRange [a,b]) = case (a,b) of 
        (HiValueNumber l, HiValueNumber u) -> hiRange l u
        _                                  -> throwError HiErrorInvalidArgument 

    e (Fun HiFunPackBytes [a]) = case a of
        HiValueList l -> do
            ints <- traverse readInt [ n | HiValueNumber n <- toList l ]  
            when (Seq.length l /= length ints || not (all (`elem` [0..255]) ints)) $ throwError HiErrorInvalidArgument 
            return $ HiValueBytes $ B.pack $ map chr $ ints
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunUnpackBytes [b]) = case b of
        HiValueBytes bs -> return $ HiValueList $ Seq.fromList $ map (HiValueNumber . fromIntegral) $ bytesToHexes $ bs
        _               -> throwError HiErrorInvalidArgument

    e (Fun HiFunEncodeUtf8 [s]) = case s of
        HiValueString t   -> return $ HiValueBytes $ B.pack $ T.unpack t
        _                 -> throwError HiErrorInvalidArgument 

    e (Fun HiFunDecodeUtf8 [b]) = case b of
        HiValueBytes bs -> return $ case T.decodeUtf8' bs of
            Left  _ -> HiValueNull
            Right t -> HiValueString t
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunZip [b]) = case b of
        HiValueBytes bs -> return $ HiValueBytes $ BL.toStrict $ 
            compressWith defaultCompressParams {compressLevel = bestCompression} $ BL.fromStrict bs
        _               -> throwError HiErrorInvalidArgument

    e (Fun HiFunUnzip [b]) = case b of
        HiValueBytes bs -> return $ HiValueBytes $ BL.toStrict $ decompress $ BL.fromStrict bs
        _               -> throwError HiErrorInvalidArgument
    
    e (Fun HiFunSerialise [a]) = return $ HiValueBytes $ BL.toStrict $ S.serialise a

    e (Fun HiFunDeserialise [b]) = case b of
        HiValueBytes bs -> return $ case S.deserialiseOrFail $ BL.fromStrict bs of
            Left _  -> HiValueNull
            Right v -> v
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunRead [d]) = case d of
        (HiValueString dir) -> return $ HiValueAction $ HiActionRead $ T.unpack dir
        _                   -> throwError HiErrorInvalidArgument

    e (Fun HiFunWrite [d, s]) = case (d, s) of
        (HiValueString dir, HiValueString s) -> 
            return $ HiValueAction $ HiActionWrite (T.unpack dir) $ bytesToNums $ B.pack $ T.unpack s
        (HiValueString dir, HiValueBytes bs) -> 
            return $ HiValueAction $ HiActionWrite (T.unpack dir) bs
        _                                    -> throwError HiErrorInvalidArgument

    e (Fun HiFunMkDir [d]) = case d of
        HiValueString d -> return $ HiValueAction $ HiActionMkDir $ T.unpack d
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunChDir [d]) = case d of
        HiValueString d -> return $ HiValueAction $ HiActionChDir $ T.unpack d
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunParseTime [t]) = case t of
        HiValueString s -> return $ case readMaybe $ T.unpack s of
            Just (t :: Time.UTCTime) -> HiValueTime t
            Nothing                  -> HiValueNull
        _           -> throwError HiErrorInvalidArgument 

    e (Fun HiFunRand [a, b]) = case (a, b) of
        (HiValueNumber a, HiValueNumber b) -> do
            l <- readInt a
            u <- readInt b
            return $ HiValueAction $ HiActionRand l u
        _                 -> throwError HiErrorInvalidArgument 

    e (Fun HiFunEcho [s]) = case s of
        HiValueString t -> return $ HiValueAction $ HiActionEcho t
        _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunKeys [d]) = case d of
        HiValueDict d -> return $ HiValueList $ Seq.fromList $ M.keys d
        _             -> throwError HiErrorInvalidArgument 

    e (Fun HiFunValues [d]) = case d of
        HiValueDict d -> return $ HiValueList $ Seq.fromList $ M.elems d
        _             -> throwError HiErrorInvalidArgument 

    e (Fun HiFunCount [x]) = let f xs = (head xs, HiValueNumber $ fromIntegral $ length xs)
                                 count = return . HiValueDict . M.fromList . map f . group . sort in
        case x of
            HiValueString s -> count $ toHiList s
            HiValueList   l -> count $ toList l
            HiValueBytes bs -> count $ map (HiValueNumber . fromIntegral) $ bytesToHexes bs  
            _               -> throwError HiErrorInvalidArgument 

    e (Fun HiFunInvert [d]) = case d of
        HiValueDict d -> let mkFst = head . map snd 
                             mkSnd = HiValueList . Seq.fromList . reverse . map fst
                             invert = map (\xs -> (mkFst xs, mkSnd xs)) . groupBy ((==) `on` snd) . sortBy (compare `on` snd)
                         in return $ HiValueDict $ M.fromList $ invert  $ M.toList d
        _                -> throwError $ HiErrorInvalidArgument   


    e (Fun HiFunFold [f, a]) = case a of
        HiValueList lst -> case toHiList lst of
            []     -> return HiValueNull
            [b]    -> return b
            (q:qs) -> case f of
                HiValueFunction f  -> foldM (\a b -> e @(Fun Lazy Binary HiExpr) $ Fun f $ map HiExprValue [ a, b]) q qs
                HiValueList     ls -> evalHiList ls (q:qs)
                HiValueString   ts -> evalHiList ts (q:qs)
                HiValueBytes    bs -> evalHiList bs (q:qs)
                _                  -> throwError HiErrorInvalidFunction
        _                -> throwError $ HiErrorInvalidArgument   

    e x = error $ show x 

hiNot :: Monad m =>  HiValue -> EvalT m Bool
hiNot (HiValueBool b) = return $ not b
hiNot _               = throwError HiErrorInvalidArgument 

hiLt :: Monad m => (HiValue, HiValue) -> EvalT m Bool
hiLt = \case
    (HiValueNumber   a, HiValueNumber   b) -> return $ a < b
    (HiValueBool     a, HiValueBool     b) -> return $ a < b
    (HiValueNumber   a, HiValueBool     b) -> return False
    (HiValueBool     a, HiValueNumber   b) -> return True
    (HiValueFunction a, HiValueFunction b) -> return $ a < b
    _                                      -> throwError HiErrorInvalidArgument 

hiEq :: Monad m => (HiValue, HiValue) -> EvalT m Bool
hiEq = return . uncurry (==)

hiRange :: Monad m => Rational -> Rational -> EvalT m HiValue
hiRange l u = return $ HiValueList $ Seq.fromList $ map (HiValueNumber . toRational) $ take i [l .. ]
  where 
      du = denominator u
      nu = numerator u
      i = round (fromRational (u - l + 1)) 

evalHiDict :: HiMonad m =>
    M.Map HiValue HiValue -> [HiValue] -> EvalT m HiValue
evalHiDict d = \case
    [v] -> do
        return $ fromMaybe HiValueNull $ M.lookup v d
    _   -> throwError HiErrorInvalidArgument 

evalPair :: (HiMonad m, Eval a, Eval b) => (a , b) -> EvalT m (HiValue, HiValue)
evalPair (a,b) = do
    a' <- e a
    b' <- e b
    return (a', b')

readInt :: (Integral a, HiMonad m) => Rational -> EvalT m a
readInt r = maybe (throwError HiErrorInvalidArgument) return $ ratioToIntegral r

-- | List-like structures :
-- | (that can be indexed/sliced/replicated/appended etc)

class ListLike a where
    toHiList         :: a         -> [HiValue]
    fromHiList       :: [HiValue] ->  HiValue
    fromHiSingletone :: [HiValue] ->  HiValue

instance ListLike (Seq.Seq HiValue) where
    toHiList = toList
    fromHiList = HiValueList . Seq.fromList
    fromHiSingletone = head

instance ListLike Text where
    toHiList = map (HiValueString . T.pack . pure) . T.unpack
    fromHiList lst = HiValueString $ T.concat [ s | HiValueString s <- lst ] 
    fromHiSingletone lst = HiValueString $ head $ [ s | HiValueString s <- lst ] 
 
instance ListLike ByteString where
    toHiList = map (HiValueBytes . B.pack . pure) . B.unpack
    fromHiList lst = HiValueBytes $ B.concat [ s | HiValueBytes s <- lst ] 
    fromHiSingletone lst = HiValueNumber $ fromIntegral $ head $ bytesToHexes $ head $ [ s | HiValueBytes s <- lst ] 

evalHiList :: forall a m. (ListLike a, HiMonad m) => a -> [HiValue] -> EvalT m HiValue
evalHiList lst args = let l = toHiList lst in case args of
    [i]        -> index @a i l
    [from, to] -> slice @a from to l
    _          -> throwError HiErrorInvalidArgument 

index :: forall a m. (ListLike a, HiMonad m) => HiValue -> [HiValue] -> EvalT m HiValue
index (HiValueNumber n) l = do
    i <- readInt n
    return $ if i >= length l || i < 0 
    then  HiValueNull
    else fromHiSingletone @a $ pure $ l !! i
index _ _ = throwError HiErrorInvalidArgument 

slice :: forall a m. (ListLike a, HiMonad m) => HiValue -> HiValue -> [HiValue] -> EvalT m HiValue
slice (HiValueNumber a) (HiValueNumber b) l = do
    from <- readInt a
    b'   <- readInt b
    let len = length l
        to = if b' < 0 then b' `mod` len else b'
    return $ fromHiList @a $ take (to - from) $ drop from l 
slice HiValueNull       (HiValueNumber b) l = do
    let len = length l 
    i <- readInt b
    return $ fromHiList @a $ if 
        | i > 0     -> take i l
        | i < 0     -> take (i `mod` len) l
        | otherwise -> []
slice (HiValueNumber a) HiValueNull       l = do
    i <- readInt a
    return $ fromHiList @a $ drop i l 
slice HiValueNull       HiValueNull       l = return $ fromHiList @a $ l
slice _                 _                 _ = throwError HiErrorInvalidArgument 

sTimes :: forall a m. (ListLike a, HiMonad m) => a -> Ratio Integer -> EvalT m HiValue
sTimes l r = do
    times <- readInt r
    when (times <= 0) $ throwError HiErrorInvalidArgument 
    return $ fromHiList @a $ Semigroup.stimes times $ toHiList l

append :: forall a m. (ListLike a, HiMonad m) => a -> a -> EvalT m HiValue
append l r = return $ fromHiList @a $ toHiList l <> toHiList r