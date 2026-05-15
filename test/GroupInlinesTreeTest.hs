{-# LANGUAGE OverloadedStrings #-}

module GroupInlinesTreeTest (tests) where

import Data.Tree (Tree (Node))
import qualified DocTree.Common as RichText (BlockNode (Caption, FigureContent), Image (..), LinkMark (..), Mark (..), TextSpan (..))
import DocTree.GroupedInlines (BlockNode (PandocBlock), DocNode (..), InlineNode (..), InlineSpan (..), TreeNode (..), toPandoc, toTree)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (runIOorExplode)
import Text.Pandoc.Builder as Pandoc (Block (..), Caption (..), Inline (..), doc, emptyCaption, fromList, link, para, str, strong, toList)
import Text.Pandoc.Definition (nullAttr, nullMeta)

treePandocBlockNode :: Pandoc.Block -> DocNode
treePandocBlockNode = TreeNode . BlockNode . PandocBlock

treeCaptionNode :: DocNode
treeCaptionNode = TreeNode (BlockNode (RichText.Caption (Pandoc.Caption Nothing [])))

treeFigureContentNode :: DocNode
treeFigureContentNode = TreeNode (BlockNode (RichText.FigureContent []))

treeTextSpansNode :: [RichText.TextSpan] -> DocNode
treeTextSpansNode = TreeNode . InlineNode . InlineContent . (fmap InlineText)

treeInlineSpansNode :: [InlineSpan] -> DocNode
treeInlineSpansNode = TreeNode . InlineNode . InlineContent

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
                      ( treeTextSpansNode $
                          [RichText.TextSpan "A heading 1" []]
                      )
                      []
                  ],
                Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      ( treeTextSpansNode $
                          [RichText.TextSpan "A paragraph" []]
                      )
                      []
                  ],
                Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      ( treeTextSpansNode $
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
                      ( treeTextSpansNode $
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
                      ( treeTextSpansNode $
                          [ RichText.TextSpan "v2" [RichText.StrongMark, RichText.LinkMark $ RichText.Link nullAttr ("https://v2editor.com/", "v2")]
                          ]
                      )
                      []
                  ]
              ]

      toTree input `shouldBe` expected

    it "handles a paragraph containing an inline image" $ do
      let input =
            Pandoc.doc $
              fromList $
                [Pandoc.Para [Pandoc.Image nullAttr [Pandoc.Str "alt text"] ("img.png", "the title")]]

          expected =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      (treeInlineSpansNode [InlineImage (RichText.Image nullAttr [Pandoc.Str "alt text"] ("img.png", "the title"))])
                      []
                  ]
              ]

      toTree input `shouldBe` expected

    it "handles a paragraph mixing text and an inline image" $ do
      let input =
            Pandoc.doc $
              fromList $
                [ Pandoc.Para
                    [ Pandoc.Str "before",
                      Pandoc.Image nullAttr [Pandoc.Str "alt"] ("img.png", ""),
                      Pandoc.Str "after"
                    ]
                ]

          expected =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      ( treeInlineSpansNode
                          [ InlineText $ RichText.TextSpan "before" [],
                            InlineImage (RichText.Image nullAttr [Pandoc.Str "alt"] ("img.png", "")),
                            InlineText $ RichText.TextSpan "after" []
                          ]
                      )
                      []
                  ]
              ]

      toTree input `shouldBe` expected

    it "handles a figure with an image body and no caption" $ do
      let input =
            Pandoc.doc $
              fromList $
                [ Pandoc.Figure
                    nullAttr
                    emptyCaption
                    [Pandoc.Plain [Pandoc.Image nullAttr [Pandoc.Str "alt"] ("img.png", "")]]
                ]

          expected =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Figure nullAttr emptyCaption [])
                  [ Node
                      treeFigureContentNode
                      [ Node
                          (treePandocBlockNode $ Pandoc.Plain [])
                          [ Node
                              (treeInlineSpansNode [InlineImage (RichText.Image nullAttr [Pandoc.Str "alt"] ("img.png", ""))])
                              []
                          ]
                      ]
                  ]
              ]

      toTree input `shouldBe` expected

    it "handles a figure with an image body and a caption" $ do
      let input =
            Pandoc.doc $
              fromList $
                [ Pandoc.Figure
                    nullAttr
                    (Pandoc.Caption Nothing [Pandoc.Plain [Pandoc.Str "the caption"]])
                    [Pandoc.Plain [Pandoc.Image nullAttr [Pandoc.Str "alt"] ("img.png", "")]]
                ]

          expected =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Figure nullAttr emptyCaption [])
                  [ Node
                      treeFigureContentNode
                      [ Node
                          (treePandocBlockNode $ Pandoc.Plain [])
                          [ Node
                              (treeInlineSpansNode [InlineImage (RichText.Image nullAttr [Pandoc.Str "alt"] ("img.png", ""))])
                              []
                          ]
                      ],
                    Node
                      treeCaptionNode
                      [ Node
                          (treePandocBlockNode $ Pandoc.Plain [])
                          [ Node (treeTextSpansNode [RichText.TextSpan "the caption" []]) []
                          ]
                      ]
                  ]
              ]

      toTree input `shouldBe` expected

    it "lifts a multi-block caption to children of a single Caption sibling" $ do
      let input =
            Pandoc.doc $
              fromList $
                [ Pandoc.Figure
                    nullAttr
                    ( Pandoc.Caption
                        Nothing
                        [ Pandoc.Para [Pandoc.Str "first"],
                          Pandoc.Para [Pandoc.Str "second"]
                        ]
                    )
                    [Pandoc.Plain [Pandoc.Image nullAttr [] ("img.png", "")]]
                ]

          expected =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Figure nullAttr emptyCaption [])
                  [ Node
                      treeFigureContentNode
                      [ Node
                          (treePandocBlockNode $ Pandoc.Plain [])
                          [ Node
                              (treeInlineSpansNode [InlineImage (RichText.Image nullAttr [] ("img.png", ""))])
                              []
                          ]
                      ],
                    Node
                      treeCaptionNode
                      [ Node
                          (treePandocBlockNode $ Pandoc.Para [])
                          [ Node (treeTextSpansNode [RichText.TextSpan "first" []]) []
                          ],
                        Node
                          (treePandocBlockNode $ Pandoc.Para [])
                          [ Node (treeTextSpansNode [RichText.TextSpan "second" []]) []
                          ]
                      ]
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
                      ( treeTextSpansNode $
                          [RichText.TextSpan "A heading 1" []]
                      )
                      []
                  ],
                Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      ( treeTextSpansNode $
                          [RichText.TextSpan "A paragraph" []]
                      )
                      []
                  ],
                Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      ( treeTextSpansNode $
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
                      ( treeTextSpansNode $
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
                      ( treeTextSpansNode $
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

    it "handles a paragraph containing an inline image" $ do
      let input =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      (treeInlineSpansNode [InlineImage (RichText.Image nullAttr [Pandoc.Str "alt text"] ("img.png", "the title"))])
                      []
                  ]
              ]

          expected =
            Pandoc.doc $
              fromList $
                [Pandoc.Para [Pandoc.Image nullAttr [Pandoc.Str "alt text"] ("img.png", "the title")]]

      output <- runIOorExplode $ toPandoc input
      output `shouldBe` expected

    it "handles a paragraph mixing text and an inline image" $ do
      let input =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Para [])
                  [ Node
                      ( treeInlineSpansNode
                          [ InlineText $ RichText.TextSpan "before" [],
                            InlineImage (RichText.Image nullAttr [Pandoc.Str "alt"] ("img.png", "")),
                            InlineText $ RichText.TextSpan "after" []
                          ]
                      )
                      []
                  ]
              ]

          expected =
            Pandoc.doc $
              fromList $
                [ Pandoc.Para
                    [ Pandoc.Str "before",
                      Pandoc.Image nullAttr [Pandoc.Str "alt"] ("img.png", ""),
                      Pandoc.Str "after"
                    ]
                ]

      output <- runIOorExplode $ toPandoc input
      output `shouldBe` expected

    it "handles a figure with an image body and no caption" $ do
      let input =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Figure nullAttr emptyCaption [])
                  [ Node
                      treeFigureContentNode
                      [ Node
                          (treePandocBlockNode $ Pandoc.Plain [])
                          [ Node
                              (treeInlineSpansNode [InlineImage (RichText.Image nullAttr [Pandoc.Str "alt"] ("img.png", ""))])
                              []
                          ]
                      ]
                  ]
              ]

          expected =
            Pandoc.doc $
              fromList $
                [ Pandoc.Figure
                    nullAttr
                    emptyCaption
                    [Pandoc.Plain [Pandoc.Image nullAttr [Pandoc.Str "alt"] ("img.png", "")]]
                ]

      output <- runIOorExplode $ toPandoc input
      output `shouldBe` expected

    it "handles a figure with an image body and a caption" $ do
      let input =
            Node
              (Root nullMeta)
              [ Node
                  (treePandocBlockNode $ Pandoc.Figure nullAttr emptyCaption [])
                  [ Node
                      treeFigureContentNode
                      [ Node
                          (treePandocBlockNode $ Pandoc.Plain [])
                          [ Node
                              (treeInlineSpansNode [InlineImage (RichText.Image nullAttr [Pandoc.Str "alt"] ("img.png", ""))])
                              []
                          ]
                      ],
                    Node
                      treeCaptionNode
                      [ Node
                          (treePandocBlockNode $ Pandoc.Plain [])
                          [ Node (treeTextSpansNode [RichText.TextSpan "the caption" []]) []
                          ]
                      ]
                  ]
              ]

          expected =
            Pandoc.doc $
              fromList $
                [ Pandoc.Figure
                    nullAttr
                    (Pandoc.Caption Nothing [Pandoc.Plain [Pandoc.Str "the caption"]])
                    [Pandoc.Plain [Pandoc.Image nullAttr [Pandoc.Str "alt"] ("img.png", "")]]
                ]

      output <- runIOorExplode $ toPandoc input
      output `shouldBe` expected