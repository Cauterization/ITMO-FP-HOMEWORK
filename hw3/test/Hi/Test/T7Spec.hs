{-# LANGUAGE CPP, BangPatterns, QuasiQuotes #-}
module Hi.Test.T7Spec (spec) where

import Text.RawString.QQ

import HW3.Base
import HW3.Action

import Hi.Test.Common
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import Control.Monad
import Text.RawString.QQ
import qualified Data.Text
import qualified Data.Set as Set

spec :: Spec
spec = do
#if HI_TEST_UPTO < 7
  emptyTest
#else
  let testEvalIO = testEvalM . unwrapHIO . Set.fromList
  let realRead s = withFile s ReadMode $ hGetContents
  let !cwd0 = unsafePerformIO getCurrentDirectory
  after_ (setCurrentDirectory cwd0) $ describe "actions" $ do
    let
      dummyApplications =
        [ [r|cd("a")|]
        , [r|mkdir("dir")|]
        , [r|read("fle")|]
        , [r|write("to", [# 30 #])|]
        ]
    it "constants" $ do
      let check s = s ~=?? Ok s
      check "cd"
      check "cd"
      check "mkdir"
      check "read"
      check "write"
      mapM_ (\s -> s ~=?? Ok s) dummyApplications
    it "run parses" $ do
      mapM_ (\s -> s ++ "!" ~=?? Ok "null") dummyApplications
    it "IO" $ do
      cwd <- getCurrentDirectory
      testEvalIO
          [AllowRead, AllowWrite]
          [r|if(true, cwd, cwd)!|]
        `shouldBe` Ok (Data.Text.unpack $ Data.Text.replace (Data.Text.pack "\\\\") (Data.Text.pack "\\") $ Data.Text.pack $ show cwd)
      testEvalIO
          [AllowRead, AllowWrite]
          [r|read("test/exec/read.test")!|]
        `shouldBe` Ok "\"303030\n\""
      testEvalIO
          [AllowRead, AllowWrite]
          [r|cd("test/exec")!|]
        `shouldBe` Ok [r|null|]
      testEvalIO
          [AllowRead, AllowWrite]
          [r|write("write.test", encode-utf8("30"))!|]
        `shouldBe` Ok [r|null|]
      testEvalIO
          [AllowRead, AllowWrite]
          [r|read("write.test")!|]
        `shouldBe` Ok [r|"30"|]
      testEvalIO
          [AllowRead, AllowWrite]
          [r|write("write.test", encode-utf8("1000-7"))!|]
        `shouldBe` Ok [r|null|]
      -- bug with lazy IO
      v1000m7 <- realRead "write.test"
      v1000m7 `shouldBe` "1000-7"
      testEvalIO
          [AllowRead, AllowWrite]
          [r|read("read.test")!|]
        `shouldBe` Ok [r|"303030\n"|]
    it "permissions" $ do
      let banned b act =  do
            testEvalIO
                (filter (/= b) [minBound..maxBound])
                act
              `shouldBe` Perm b
      banned AllowRead [r|read("read.test")!|]
      banned AllowRead [r|cwd!|]
      banned AllowRead [r|cd("..")!|]
      banned AllowWrite [r|write("write.test", [# 65 #])!|]
      banned AllowWrite [r|mkdir("testmk")!|]
    it "read-string" $ do
      testEvalIO
          [AllowWrite]
          [r|write("test/exec/write.test", [# ff #])!|]
        `shouldBe` Ok [r|null|]
      testEvalIO
          [AllowRead]
          [r|read("test/exec/write.test")!|]
        `shouldBe` Ok "[# ff #]"
    it "lazy" $ do
      testEvalIO
          [AllowWrite]
          -- move bang out of if to additionally test propogation
          [r|if(false, write("test/exec/write.test", "nonlazy")!, write("test/exec/write.test", encode-utf8("lazy")))!|]
        `shouldBe` Ok [r|null|]
      lz <- realRead "test/exec/write.test"
      lz `shouldBe` "lazy"
      testEvalIO
          [AllowWrite]
          -- meve bang out of if to additionally test propogation
          [r|if(true, write("test/exec/write.test", encode-utf8("for sure"))!, write("test/exec/write.test", "oops leftmost")!)|]
        `shouldBe` Ok [r|null|]
      sure <- realRead "test/exec/write.test"
      sure `shouldBe` "for sure"
    it "multiple-run" $ do
      testEvalIO
          [AllowTime]
          [r|now!!|]
        `shouldBe` EvalError HiErrorInvalidFunction
#endif
