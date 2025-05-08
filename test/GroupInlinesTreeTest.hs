{-# LANGUAGE OverloadedStrings #-}

module GroupInlinesTreeTest (tests) where

import Data.Tree (Tree (Node), unfoldForest)
import DocTree.Common (TextSpan (..))
import DocTree.GroupedInlines (BlockNode (..), DocNode (..), InlineNode (..), TreeNode (..), toTree)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc.Builder as Pandoc (Block (..), Inline (..), Many (unMany), bulletList, doc, emph, fromList, header, link, orderedList, para, plain, str, strong, toList)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (nullAttr)

treePandocBlockNode :: Pandoc.Block -> DocNode
treePandocBlockNode = TreeNode . BlockNode . PandocBlock

treeInlineNode :: [TextSpan] -> DocNode
treeInlineNode = TreeNode . InlineNode . InlineContent

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

  it "handles a simple document with a heading and a couple of paragraphs" $ do
    let input =
          Pandoc.doc $
            fromList $
              [ Pandoc.Header 1 nullAttr [Pandoc.Str "A heading 1"],
                Pandoc.Para [Pandoc.Str "A paragraph"],
                Pandoc.Para [Pandoc.Str "Another paragraph"]
              ]

        -- Note that the pandoc blocks in the tree representation always have empty children.
        -- Their inline content (children) is modeled as separate nodes in the tree.
        expected =
          Node
            Root
            [ Node
                (treePandocBlockNode $ Pandoc.Header 1 nullAttr [])
                [ Node
                    ( treeInlineNode $
                        [TextSpan "A heading 1" []]
                    )
                    []
                ],
              Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node
                    ( treeInlineNode $
                        [TextSpan "A paragraph" []]
                    )
                    []
                ],
              Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node
                    ( treeInlineNode $
                        [TextSpan "Another paragraph" []]
                    )
                    []
                ]
            ]

    toTree input `shouldBe` expected