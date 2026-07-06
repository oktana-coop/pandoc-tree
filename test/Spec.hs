import qualified GroupedInlinesTree.PandocToTree as PandocToTree (tests)
import qualified GroupedInlinesTree.RoundTrip as RoundTrip (tests)
import qualified GroupedInlinesTree.TreeToPandoc as TreeToPandoc (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  pandocToTreeTests <- PandocToTree.tests
  treeToPandocTests <- TreeToPandoc.tests
  roundTripTests <- RoundTrip.tests
  defaultMain $
    testGroup
      "Tests"
      [ testGroup
          "Grouped Inlines Tree"
          [pandocToTreeTests, treeToPandocTests, roundTripTests]
      ]
