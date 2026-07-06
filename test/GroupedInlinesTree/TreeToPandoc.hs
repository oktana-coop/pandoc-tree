{-# LANGUAGE OverloadedStrings #-}

module GroupedInlinesTree.TreeToPandoc (tests) where

import Data.Tree (Tree (Node))
import qualified DocTree.Common as RichText (Image (..), LinkMark (..), Mark (..), TextSpan (..))
import DocTree.GroupedInlines (DocNode (..), InlineSpan (..), toPandoc)
import GroupedInlinesTree.Utils (treeCaptionNode, treeFigureContentNode, treeInlineSpansNode, treePandocBlockNode, treeTextSpansNode)
import Test.Hspec (Spec, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (runIOorExplode)
import Text.Pandoc.Builder as Pandoc (Block (..), Caption (..), Inline (..), doc, emptyCaption, fromList, link, para, str, strong, toList)
import Text.Pandoc.Definition (nullAttr, nullMeta)

tests :: IO TestTree
tests = testSpec "Grouped Inlines Tree → Pandoc" spec

spec :: Spec
spec = do
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
