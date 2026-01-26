{-# LANGUAGE OverloadedStrings #-}

module GroupInlinesTreeTest (tests) where

import Data.Tree (Tree (Node))
import qualified DocTree.Common as RichText (LinkMark (..), Mark (..), TextSpan (..))
import DocTree.GroupedInlines (BlockNode (..), DocNode (..), InlineNode (..), InlineSpan (..), TreeNode (..), toTree)
import Test.Hspec (Spec, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc.Builder as Pandoc (Block (..), Inline (..), doc, fromList, link, para, str, strong, toList)
import Text.Pandoc.Definition (nullAttr)

treePandocBlockNode :: Pandoc.Block -> DocNode
treePandocBlockNode = TreeNode . BlockNode . PandocBlock

treeInlineNode :: [RichText.TextSpan] -> DocNode
treeInlineNode = TreeNode . InlineNode . InlineContent . (fmap InlineText)

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
                        [RichText.TextSpan "A heading 1" []]
                    )
                    []
                ],
              Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node
                    ( treeInlineNode $
                        [RichText.TextSpan "A paragraph" []]
                    )
                    []
                ],
              Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node
                    ( treeInlineNode $
                        [RichText.TextSpan "Another paragraph" []]
                    )
                    []
                ]
            ]

    toTree input `shouldBe` expected

  it "groups text spans with different marks under a single inline node" $ do
    let input =
          Pandoc.doc $
            fromList $
              concat
                [ toList $
                    Pandoc.para $
                      fromList $
                        concat
                          [ toList $ Pandoc.str "Some plain text followed by ",
                            toList $ Pandoc.strong $ Pandoc.str "strong text",
                            toList $ Pandoc.str " and a link: ",
                            toList $ Pandoc.link "https://automerge.org/" "Automerge" $ Pandoc.str "Automerge"
                          ]
                ]

        expected =
          Node
            Root
            [ Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node
                    ( treeInlineNode $
                        [ RichText.TextSpan "Some plain text followed by " [],
                          RichText.TextSpan "strong text" [RichText.StrongMark],
                          RichText.TextSpan " and a link: " [],
                          RichText.TextSpan "Automerge" [RichText.LinkMark $ RichText.Link nullAttr ("https://automerge.org/", "Automerge")]
                        ]
                    )
                    []
                ]
            ]

    toTree input `shouldBe` expected