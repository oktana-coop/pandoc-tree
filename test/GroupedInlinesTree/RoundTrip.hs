{-# LANGUAGE OverloadedStrings #-}

module GroupedInlinesTree.RoundTrip (tests) where

import qualified Data.Text as T
import DocTree.GroupedInlines (toTree)
import GroupedInlinesTree.Utils (toPandocOrFail)
import Test.Hspec (Spec, it, shouldReturn)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (WrapOption (WrapPreserve), def, readMarkdown, readerExtensions, runIOorExplode, writeMarkdown, writerExtensions, writerWrapText)
import Text.Pandoc.Extensions (pandocExtensions)

tests :: IO TestTree
tests = testSpec "Markdown → Tree → Markdown round trip" spec

spec :: Spec
spec = do
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

-- Reads the markdown, takes the document through the tree and back, and writes it.
roundTripMarkdown :: T.Text -> IO T.Text
roundTripMarkdown markdown = do
  pandoc <- runIOorExplode $ readMarkdown def {readerExtensions = pandocExtensions} markdown
  roundTripped <- toPandocOrFail (toTree pandoc)
  runIOorExplode $ writeMarkdown def {writerExtensions = pandocExtensions, writerWrapText = WrapPreserve} roundTripped