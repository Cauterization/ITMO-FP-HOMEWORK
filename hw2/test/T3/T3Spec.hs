module T3Spec where


import Test.TAnnotated
import Test.TExcept
import Test.TList
import Test.TOption
import Test.Tasty (TestTree, testGroup)


tests :: IO TestTree
tests = do
    option <- hspecOption
    annotated <- hspecAnnotated
    except <- hspecExcept
    list <- hspecList
    -- fun <- funProp
    return $ testGroup "HW2.T3" [option, annotated, except, list]
