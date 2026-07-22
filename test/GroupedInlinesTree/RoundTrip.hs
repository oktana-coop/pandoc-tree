{-# LANGUAGE OverloadedStrings #-}

module GroupedInlinesTree.RoundTrip (tests) where

import qualified Data.Text as T
import DocTree.GroupedInlines (toPandoc, toTree)
import Test.Hspec (Spec, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (WrapOption (WrapPreserve), def, readMarkdown, readerExtensions, runIOorExplode, writeMarkdown, writerExtensions, writerWrapText)
import Text.Pandoc.Extensions (Extension (Ext_smart), enableExtension, pandocExtensions)

tests :: IO TestTree
tests = testSpec "Markdown → Tree → Markdown round trip" spec

spec :: Spec
spec = do
  it "preserves quoted text when the reader has Ext_smart enabled" $ do
    let markdown = "She replied \"yes\" to the question, then 'maybe'.\n"
        smartExtensions = enableExtension Ext_smart pandocExtensions

    output <- runIOorExplode $ do
      pandoc <- readMarkdown def {readerExtensions = smartExtensions} markdown
      roundTripped <- toPandoc $ toTree pandoc
      writeMarkdown def {writerExtensions = smartExtensions, writerWrapText = WrapPreserve} roundTripped

    output `shouldBe` markdown

  -- Curly quote characters parse as `Quoted` just like straight ones, and the writer
  -- emits straight quotes for them, so curly sources normalize (not byte-stable).
  -- Guillemets are not quote delimiters for Pandoc and pass through untouched.
  it "normalizes typographic quote characters to straight quotes on write" $ do
    -- The type annotation pins the overloaded literal: unlike above, `markdown` is only
    -- passed to `readMarkdown`, which is polymorphic in its input (`ToSources`).
    let markdown = "Curly “double” and ‘single’ quotes, and «guillemets».\n" :: T.Text
        normalized = "Curly \"double\" and 'single' quotes, and «guillemets».\n"
        smartExtensions = enableExtension Ext_smart pandocExtensions

    output <- runIOorExplode $ do
      pandoc <- readMarkdown def {readerExtensions = smartExtensions} markdown
      roundTripped <- toPandoc $ toTree pandoc
      writeMarkdown def {writerExtensions = smartExtensions, writerWrapText = WrapPreserve} roundTripped

    output `shouldBe` normalized

  -- Blocks without a dedicated tree shape (tables, line blocks, definition lists)
  -- are kept intact as tree leaves, so the tree round trip must be a no-op.
  -- The expectation is derived by writing the directly-read AST, which keeps the
  -- assertion independent of the writer's formatting choices.
  it "round-trips a document containing a table unchanged" $ do
    let markdown = "Intro paragraph.\n\n| Name | Version |\n|------|---------|\n| foo | 1.0 |\n\nOutro paragraph.\n" :: T.Text

    (expected, output) <- roundTripAgainstDirectWrite markdown
    output `shouldBe` expected

  it "round-trips a document containing a line block unchanged" $ do
    let markdown = "| first line\n| second line\n" :: T.Text

    (expected, output) <- roundTripAgainstDirectWrite markdown
    output `shouldBe` expected

  it "round-trips a document containing a definition list unchanged" $ do
    let markdown = "term\n: definition\n" :: T.Text

    (expected, output) <- roundTripAgainstDirectWrite markdown
    output `shouldBe` expected

-- Reads the markdown, then writes it both directly and after a tree round trip.
roundTripAgainstDirectWrite :: T.Text -> IO (T.Text, T.Text)
roundTripAgainstDirectWrite markdown = runIOorExplode $ do
  pandoc <- readMarkdown def {readerExtensions = pandocExtensions} markdown
  expected <- writeMarkdown writerOpts pandoc
  roundTripped <- toPandoc $ toTree pandoc
  output <- writeMarkdown writerOpts roundTripped
  return (expected, output)
  where
    writerOpts = def {writerExtensions = pandocExtensions, writerWrapText = WrapPreserve}