{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module HW3.Action where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Exception (Exception, throw, throwIO)
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString qualified as B
import Data.Functor (($>))
import Data.Set (Set, member)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding qualified as T
import Data.Time qualified as Time
import GHC.IO.Exception (IOErrorType (PermissionDenied))

import System.Directory
import System.IO
import System.Random

import HW3.Base

-- | Various action permissions 

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show, Eq, Ord)

instance Exception PermissionException

-- | A monad in which various hi-actions can be performed

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
    fmap = liftM

instance Applicative HIO where
    pure = return
    (<*>) = ap

instance Monad HIO where
    return a = HIO $ const $ return a
    ma >>= k = HIO $ \perm -> do
        a <- (runHIO ma perm)
        runHIO (k a) perm

instance MonadIO HIO where
    liftIO = HIO . const

-- | Permission check

required :: HiPermission -> HIO ()
required perm = HIO $ \perms -> unless (perm `member` perms) $ throwIO $ PermissionRequired perm

instance HiMonad HIO where

    runAction = \case

        HiActionCwd -> required AllowRead >> hiCwd

        HiActionChDir fp -> required AllowRead >> hiChDir fp

        HiActionRead fp -> required AllowRead >> hiRead fp

        HiActionMkDir fp -> required AllowWrite >> hiMkDir fp

        HiActionWrite fp bs -> required AllowWrite >> hiWrite fp bs

        HiActionNow -> required AllowTime >> hiNow

        HiActionRand l u -> hiRand l u

        HiActionEcho t -> required AllowWrite >> hiEcho t

-- | Get current working directory

hiCwd :: HIO HiValue
hiCwd = liftIO $ do
    c <- getCurrentDirectory
    HiValueString . T.pack . concat <$> listDirectory c

-- | Change current working directory

hiChDir :: FilePath -> HIO HiValue
hiChDir fp = liftIO $ do
    setCurrentDirectory fp 
    return HiValueNull

-- | Read a file/dir

hiRead :: FilePath -> HIO HiValue
hiRead fp = liftIO $ do
    exist <- doesFileExist fp
    if exist 
    then do
        withFile fp ReadMode $ \handle -> do
            bytes <- B.hGetContents handle 
            return $ case T.decodeUtf8' bytes of
                Left _  -> HiValueBytes bytes 
                Right r -> HiValueString r
    else do
        dirs <- filterM (doesDirectoryExist) <$> listDirectory fp
        HiValueList . Seq.fromList . map (HiValueString . T.pack) <$> dirs

-- | Create a new directory

hiMkDir :: FilePath -> HIO HiValue
hiMkDir fp = liftIO $ do
    createDirectoryIfMissing False fp 
    return $ HiValueNull

-- | Write to a file

hiWrite :: FilePath -> BC8.ByteString -> HIO HiValue
hiWrite fp bs = liftIO $ do
    withFile fp WriteMode $ \handle -> B.hPut handle bs $> HiValueNull 

-- | Current time

hiNow :: HIO HiValue
hiNow = liftIO $ do
    t <- Time.getCurrentTime
    return $ HiValueTime t 

-- | Random number

hiRand :: Int -> Int -> HIO HiValue
hiRand l u = do
    r <- randomRIO (l, u)
    return $ HiValueNumber $ toRational r  

-- | Echo a string

hiEcho :: T.Text -> HIO HiValue
hiEcho t = liftIO $ do
    T.putStrLn t
    return HiValueNull

