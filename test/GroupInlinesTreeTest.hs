module GroupInlinesTreeTest (tests) where

import Data.Tree (Tree (Node), unfoldForest)
import DocTree.GroupedInlines (DocNode (..), toTree)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc.Builder as Pandoc (bulletList, doc, emph, fromList, header, link, orderedList, para, plain, str, strong, toList)
import Text.Pandoc.Class (runIO)

tests :: IO TestTree
tests = do
  hspecTests <- testSpec "hspec" spec
  return $ testGroup "Reader" [hspecTests]

spec :: Spec
spec = do
  it "handles an empty Pandoc document" $ do
    let input = Pandoc.doc $ fromList []
        expected = Node Root []

    toTree input `shouldBe` expected
