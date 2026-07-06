{-# LANGUAGE OverloadedStrings #-}

module GroupedInlinesTree.PandocToTree (tests) where

import Data.Tree (Tree (Node))
import qualified DocTree.Common as RichText (Image (..), LinkMark (..), Mark (..), TextSpan (..))
import DocTree.GroupedInlines (DocNode (..), InlineSpan (..), toTree)
import GroupedInlinesTree.Utils (treeCaptionNode, treeFigureContentNode, treeInlineSpansNode, treePandocBlockNode, treeTextSpansNode)
import Test.Hspec (Spec, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc.Builder as Pandoc (Block (..), Caption (..), Inline (..), QuoteType (..), doc, emptyCaption, fromList, link, para, str, strong, toList)
import Text.Pandoc.Definition (nullAttr, nullMeta)

tests :: IO TestTree
tests = testSpec "Pandoc → Grouped Inlines Tree" spec

spec :: Spec
spec = do
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

  it "handles Pandoc double quotes" $ do
    let input =
          Pandoc.doc $
            fromList $
              [ Pandoc.Para
                  [ Pandoc.Str "She",
                    Pandoc.Space,
                    Pandoc.Str "replied",
                    Pandoc.Space,
                    Pandoc.Quoted Pandoc.DoubleQuote [Pandoc.Str "yes"],
                    Pandoc.Space,
                    Pandoc.Str "to",
                    Pandoc.Space,
                    Pandoc.Str "the",
                    Pandoc.Space,
                    Pandoc.Str "question"
                  ]
              ]

        expected =
          Node
            (Root nullMeta)
            [ Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node (treeTextSpansNode [RichText.TextSpan "She replied “yes” to the question" []]) []
                ]
            ]

    toTree input `shouldBe` expected

  it "handles Pandoc single quotes" $ do
    let input =
          Pandoc.doc $
            fromList $
              [Pandoc.Para [Pandoc.Str "then", Pandoc.Space, Pandoc.Str "added", Pandoc.Space, Pandoc.Quoted Pandoc.SingleQuote [Pandoc.Str "maybe"]]]

        expected =
          Node
            (Root nullMeta)
            [ Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node (treeTextSpansNode [RichText.TextSpan "then added ‘maybe’" []]) []
                ]
            ]

    toTree input `shouldBe` expected

  -- `"a **bold** claim"`: the quote characters merge with the unmarked text around
  -- the strong word; only the strong word carries the mark.
  it "marks only the strong text inside quotes, leaving the quote characters and the rest unmarked" $ do
    let input =
          Pandoc.doc $
            fromList $
              [Pandoc.Para [Pandoc.Quoted Pandoc.DoubleQuote [Pandoc.Str "a", Pandoc.Space, Pandoc.Strong [Pandoc.Str "bold"], Pandoc.Space, Pandoc.Str "claim"]]]

        expected =
          Node
            (Root nullMeta)
            [ Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node
                    ( treeTextSpansNode
                        [ RichText.TextSpan "“a " [],
                          RichText.TextSpan "bold" [RichText.StrongMark],
                          RichText.TextSpan " claim”" []
                        ]
                    )
                    []
                ]
            ]

    toTree input `shouldBe` expected

  -- `'**wow**'`: the strong emphasis covers the whole quoted content, but the quote
  -- characters belong to the `Quoted` wrapper and must stay unmarked.
  it "does not extend a mark covering the whole quoted content to the quote characters" $ do
    let input =
          Pandoc.doc $
            fromList $
              [Pandoc.Para [Pandoc.Quoted Pandoc.SingleQuote [Pandoc.Strong [Pandoc.Str "wow"]]]]

        expected =
          Node
            (Root nullMeta)
            [ Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node
                    ( treeTextSpansNode
                        [ RichText.TextSpan "‘" [],
                          RichText.TextSpan "wow" [RichText.StrongMark],
                          RichText.TextSpan "’" []
                        ]
                    )
                    []
                ]
            ]

    toTree input `shouldBe` expected

  -- `**'wow'** *"sure"*`: here the quotes are nested inside the emphasis, so the
  -- quote characters carry the mark too and each quoted phrase merges into one span.
  it "marks the quote characters when the quotes are nested inside bold or italics" $ do
    let input =
          Pandoc.doc $
            fromList $
              [ Pandoc.Para
                  [ Pandoc.Strong [Pandoc.Quoted Pandoc.SingleQuote [Pandoc.Str "wow"]],
                    Pandoc.Space,
                    Pandoc.Emph [Pandoc.Quoted Pandoc.DoubleQuote [Pandoc.Str "sure"]]
                  ]
              ]

        expected =
          Node
            (Root nullMeta)
            [ Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node
                    ( treeTextSpansNode
                        [ RichText.TextSpan "‘wow’" [RichText.StrongMark],
                          RichText.TextSpan " " [],
                          RichText.TextSpan "“sure”" [RichText.EmphMark]
                        ]
                    )
                    []
                ]
            ]

    toTree input `shouldBe` expected

  it "converts soft breaks to spaces and line breaks to newlines" $ do
    let input =
          Pandoc.doc $
            fromList $
              [Pandoc.Para [Pandoc.Str "foo", Pandoc.SoftBreak, Pandoc.Str "bar", Pandoc.LineBreak, Pandoc.Str "baz"]]

        expected =
          Node
            (Root nullMeta)
            [ Node
                (treePandocBlockNode $ Pandoc.Para [])
                [ Node (treeTextSpansNode [RichText.TextSpan "foo bar\nbaz" []]) []
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
