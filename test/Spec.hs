import GroupInlinesTreeTest (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  groupedInlinesTreeTests <- GroupInlinesTreeTest.tests
  defaultMain $ testGroup "Tests" [groupedInlinesTreeTests]
