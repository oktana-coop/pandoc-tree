{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module GroupedInlinesTree.RoundTrip (tests) where

import Control.Monad ((>=>))
import qualified Data.Map as M
import qualified Data.Text as T
import DocTree.GroupedInlines (toTree)
import GroupedInlinesTree.Utils (toPandocOrFail)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (WrapOption (WrapPreserve), compileDefaultTemplate, def, readMarkdown, readerExtensions, runIOorExplode, writeMarkdown, writerExtensions, writerTemplate, writerWrapText)
import Text.Pandoc.Builder as Pandoc (Block (..), Inline (..), Meta (..), MetaValue (..), Pandoc (..))
import Text.Pandoc.Extensions (pandocExtensions)

tests :: IO TestTree
tests = testSpec "Round trip through the tree" spec

spec :: Spec
spec = do
  describe "Body" $ do
    it "preserves quoted text when the reader has Ext_smart enabled" $ do
      let markdown = "She replied \"yes\" to the question, then 'maybe'.\n"

      roundTripMarkdown markdown `shouldReturn` markdown

    -- Curly quote characters parse as `Quoted` just like straight ones, and the writer
    -- emits straight quotes for them, so curly sources normalize (not byte-stable).
    -- Guillemets are not quote delimiters for Pandoc and pass through untouched.
    it "normalizes typographic quote characters to straight quotes on write" $ do
      let markdown = "Curly “double” and ‘single’ quotes, and «guillemets».\n"
          normalized = "Curly \"double\" and 'single' quotes, and «guillemets».\n"

      roundTripMarkdown markdown `shouldReturn` normalized

    -- Blocks without a dedicated tree shape (tables, line blocks, definition lists)
    -- are kept intact as tree leaves, so the tree round trip must be a no-op.
    it "round-trips a document containing a table unchanged" $ do
      let markdown = "Intro paragraph.\n\n  Name   Version\n  ------ ---------\n  foo    1.0\n\nOutro paragraph.\n"

      roundTripMarkdown markdown `shouldReturn` markdown

    it "round-trips a document containing a line block unchanged" $ do
      let markdown = "| first line\n| second line\n"

      roundTripMarkdown markdown `shouldReturn` markdown

    it "round-trips a document containing a definition list unchanged" $ do
      let markdown = "term\n: definition\n"

      roundTripMarkdown markdown `shouldReturn` markdown

    it "round-trips inlines the tree doesn't model unchanged" $ do
      let markdown = "Energy $E = mc^2$, ~~struck~~, x^2^, H~2~O, [a span]{.note} and `<b>`{=html}bold`</b>`{=html}.\n"

      roundTripMarkdown markdown `shouldReturn` markdown

  describe "Metadata" $ do
    it "round-trips each kind of value" $ do
      let input =
            withMetadata
              [ ("abstract", MetaBlocks [Para [Str "One."], Para [Str "Two."]]),
                ("draft", MetaBool True),
                ("tags", MetaList [MetaInlines [Str "a"], MetaInlines [Str "b"]]),
                ("title", MetaInlines [Str "A ", Emph [Str "short"], Str " title"]),
                ("venue", MetaMap $ M.fromList [("city", MetaInlines [Str "Athens"])]),
                ("version", MetaString "1.10")
              ]
              [Para [Str "The body."]]

      roundTrip input `shouldReturn` input

    it "round-trips nested maps and lists" $ do
      let markdown =
            """
            ---
            authors:
              - name: Ada
                affiliations: [ETH, EPFL]
              - name: Bob
            matrix: [[true, false], []]
            ---
            """
      expected <- roundTripMarkdownWithoutTree markdown

      roundTripMarkdown markdown `shouldReturn` expected

    it "round-trips empty values" $ do
      let input =
            withMetadata
              [ ("blocks", MetaBlocks []),
                ("inlines", MetaInlines []),
                ("list", MetaList []),
                ("map", MetaMap M.empty),
                ("string", MetaString "")
              ]
              []

      roundTrip input `shouldReturn` input

    it "round-trips notes in an inline and a block value next to the body's notes" $ do
      let markdown =
            """
            ---
            title: Title^[Title note.]
            abstract: |
              Abstract^[Abstract note.]
            ---

            Body^[Body note.]
            """
      expected <- roundTripMarkdownWithoutTree markdown

      roundTripMarkdown markdown `shouldReturn` expected

-- Reads the markdown, takes the document through the tree and back, and writes it.
roundTripMarkdown :: T.Text -> IO T.Text
roundTripMarkdown = readMarkdownDocument >=> roundTrip >=> writeMarkdownDocument

roundTripMarkdownWithoutTree :: T.Text -> IO T.Text
roundTripMarkdownWithoutTree = readMarkdownDocument >=> writeMarkdownDocument

roundTrip :: Pandoc -> IO Pandoc
roundTrip = toPandocOrFail . toTree

readMarkdownDocument :: T.Text -> IO Pandoc
readMarkdownDocument = runIOorExplode . readMarkdown def {readerExtensions = pandocExtensions}

writeMarkdownDocument :: Pandoc -> IO T.Text
writeMarkdownDocument pandoc = runIOorExplode $ do
  -- We need a template to write the front matter.
  template <- compileDefaultTemplate "markdown"
  writeMarkdown def {writerExtensions = pandocExtensions, writerWrapText = WrapPreserve, writerTemplate = Just template} pandoc

-- The values' inlines are written the way the tree reads them back (e.g. a space joined to its text), so the round
-- trip gives back the same document.
withMetadata :: [(T.Text, MetaValue)] -> [Block] -> Pandoc
withMetadata entries = Pandoc (Meta $ M.fromList entries)
