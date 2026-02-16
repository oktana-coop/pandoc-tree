{-# LANGUAGE OverloadedStrings #-}

module GroupInlinesTreeTest (tests) where

import Data.Tree (Tree (Node))
import qualified DocTree.Common as RichText (LinkMark (..), Mark (..), TextSpan (..))
import DocTree.GroupedInlines (BlockNode (..), DocNode (..), InlineNode (..), InlineSpan (..), TreeNode (..), toPandoc, toTree)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (runIOorExplode)
import Text.Pandoc.Builder as Pandoc (Block (..), Inline (..), doc, fromList, link, para, str, strong, toList)
import Text.Pandoc.Definition (nullAttr, nullMeta)

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
  describe "Pandoc → Grouped Inlines Tree" $ do
    it "handles an empty Pandoc document" $ do
      let input = Pandoc.doc $ fromList []
          expected = Node (Root nullMeta) []

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
              (Root nullMeta)
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
              (Root nullMeta)
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

    it "maintains marks order in case there is more than one" $ do
      let input =
            Pandoc.doc $
              fromList $
                concat
                  [ toList $
                      Pandoc.para $
                        fromList $
                          concat
                            [ toList $ Pandoc.strong $ Pandoc.link "https://v2editor.com/" "v2" $ Pandoc.str "v2"
                            ]
                  ]

      let expected =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      ( treeInlineNode $
                          [ RichText.TextSpan "v2" [RichText.StrongMark, RichText.LinkMark $ RichText.Link nullAttr ("https://v2editor.com/", "v2")]
                          ]
                      )
                      []
                  ]
              ]

      toTree input `shouldBe` expected

  describe "Pandoc → Grouped Inlines Tree" $ do
    it "handles a tree containing just a root node" $ do
      let input = Node (Root nullMeta) []
          expected = Pandoc.doc $ fromList []

      output <- runIOorExplode $ toPandoc input
      output `shouldBe` expected

    it "handles a simple document with a heading and a couple of paragraphs" $ do
      let input =
            Node
              (Root nullMeta)
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

      let expected =
            Pandoc.doc $
              fromList $
                [ Pandoc.Header 1 nullAttr [Pandoc.Str "A heading 1"],
                  Pandoc.Para [Pandoc.Str "A paragraph"],
                  Pandoc.Para [Pandoc.Str "Another paragraph"]
                ]

      output <- runIOorExplode $ toPandoc input
      output `shouldBe` expected

    it "assigns an inline node's children to the parent block" $ do
      let input =
            Node
              (Root nullMeta)
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

      let expected =
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

      output <- runIOorExplode $ toPandoc input
      output `shouldBe` expected

    it "maintains marks order in case there is more than one" $ do
      let input =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      ( treeInlineNode $
                          [ RichText.TextSpan "v2" [RichText.StrongMark, RichText.LinkMark $ RichText.Link nullAttr ("https://v2editor.com/", "v2")]
                          ]
                      )
                      []
                  ]
              ]

      let expected =
            Pandoc.doc $
              fromList $
                concat
                  [ toList $
                      Pandoc.para $
                        fromList $
                          concat
                            [ toList $ Pandoc.strong $ Pandoc.link "https://v2editor.com/" "v2" $ Pandoc.str "v2"
                            ]
                  ]

      output <- runIOorExplode $ toPandoc input
      output `shouldBe` expected