module GroupedInlinesTree.Utils (toPandocOrFail, treeCaptionNode, treeFigureContentNode, treeInlineSpansNode, treePandocBlockNode, treeTextSpansNode) where

import Data.Tree (Tree)
import qualified DocTree.Common as RichText (BlockNode (Caption, FigureContent), TextSpan)
import DocTree.GroupedInlines (BlockNode (PandocBlock), DocNode (..), InlineNode (..), InlineSpan (..), RichTextNode (..), toPandoc)
import qualified Text.Pandoc.Builder as Pandoc (Block, Caption (..), Pandoc)

toPandocOrFail :: Tree DocNode -> IO Pandoc.Pandoc
toPandocOrFail = either (fail . show) pure . toPandoc

treePandocBlockNode :: Pandoc.Block -> DocNode
treePandocBlockNode = RichTextNode . BlockNode . PandocBlock

treeCaptionNode :: DocNode
treeCaptionNode = RichTextNode (BlockNode (RichText.Caption (Pandoc.Caption Nothing [])))

treeFigureContentNode :: DocNode
treeFigureContentNode = RichTextNode (BlockNode (RichText.FigureContent []))

treeTextSpansNode :: [RichText.TextSpan] -> DocNode
treeTextSpansNode = RichTextNode . InlineNode . InlineContent . (fmap InlineText)

treeInlineSpansNode :: [InlineSpan] -> DocNode
treeInlineSpansNode = RichTextNode . InlineNode . InlineContent
