module DocTree.GroupedInlines (BlockNode (..), InlineSpan (..), DocNode (..), TreeNode (..), toTree) where

import qualified Data.Text as T
import Data.Tree (Tree (Node), unfoldForest)
import DocTree.Common (BlockNode (..), InlineSpan (..), LinkMark (..), Mark (..), TextSpan (..))
import Text.Pandoc.Definition as Pandoc (Block (..), Inline (..), Pandoc (..))

data TreeNode = BlockNode BlockNode | InlineNode [InlineSpan] deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)

toTree :: Pandoc.Pandoc -> Tree DocNode
toTree (Pandoc.Pandoc _ blocks) = Node Root $ unfoldForest treeNodeUnfolder $ map (BlockNode . PandocBlock) blocks

treeNodeUnfolder :: TreeNode -> (DocNode, [TreeNode])
treeNodeUnfolder (BlockNode blockNode) = blockTreeNodeUnfolder blockNode
treeNodeUnfolder (InlineNode inlineSpans) = inlineTreeNodeUnfolder inlineSpans

blockTreeNodeUnfolder :: BlockNode -> (DocNode, [TreeNode])
blockTreeNodeUnfolder (PandocBlock block) = case block of
  Pandoc.Plain inlines -> ((TreeNode . BlockNode . PandocBlock . Pandoc.Plain) [], [buildInlineNode inlines])
  Pandoc.Para inlines -> ((TreeNode . BlockNode . PandocBlock . Pandoc.Para) [], [buildInlineNode inlines])
  Pandoc.Header level attrs inlines -> (TreeNode . BlockNode $ PandocBlock $ Pandoc.Header level attrs [], [buildInlineNode inlines])
  Pandoc.CodeBlock attrs text -> (TreeNode . BlockNode $ PandocBlock $ Pandoc.CodeBlock attrs T.empty, [InlineNode $ [InlineText $ TextSpan {value = text, marks = []}]])
  Pandoc.BulletList items -> ((TreeNode . BlockNode . PandocBlock . Pandoc.BulletList) [], map (BlockNode . ListItem) items)
  Pandoc.OrderedList attrs items -> (TreeNode $ BlockNode $ PandocBlock $ Pandoc.OrderedList attrs [], map (BlockNode . ListItem) items)
  Pandoc.BlockQuote children -> ((TreeNode . BlockNode . PandocBlock . Pandoc.BlockQuote) [], map (BlockNode . PandocBlock) children)
  _ -> undefined
blockTreeNodeUnfolder (ListItem children) = ((TreeNode . BlockNode . ListItem) [], map (BlockNode . PandocBlock) children)
blockTreeNodeUnfolder (NoteContent noteId children) = (TreeNode $ BlockNode $ NoteContent noteId [], map (BlockNode . PandocBlock) children)

buildInlineNode :: [Pandoc.Inline] -> TreeNode
buildInlineNode inlines = InlineNode $ pandocInlinesToSpans inlines

pandocInlinesToSpans :: [Pandoc.Inline] -> [InlineSpan]
pandocInlinesToSpans = mergeSameMarkSpans . foldMap inlineToSpans

mergeSameMarkSpans :: [InlineSpan] -> [InlineSpan]
mergeSameMarkSpans = foldr mergeOrAppendAdjacent []
  where
    mergeOrAppendAdjacent :: InlineSpan -> [InlineSpan] -> [InlineSpan]
    mergeOrAppendAdjacent x [] = [x]
    -- We only merge if the adjacent spans are **text** spans and they have the same marks.
    mergeOrAppendAdjacent (InlineText xTextSpan) (InlineText firstOrRestTextSpan : rest)
      | marks xTextSpan == marks firstOrRestTextSpan =
          InlineText (xTextSpan <> firstOrRestTextSpan) : rest
    mergeOrAppendAdjacent x rest = x : rest

inlineToSpans :: Pandoc.Inline -> [InlineSpan]
inlineToSpans inline = case inline of
  Pandoc.Str str -> [InlineText $ TextSpan str []]
  Pandoc.Space -> [InlineText $ TextSpan (T.pack " ") []]
  Pandoc.Strong inlines -> addMark StrongMark inlines
  Pandoc.Emph inlines -> addMark EmphMark inlines
  Pandoc.Link attrs inlines target -> addMark (LinkMark $ DocTree.Common.Link attrs target) inlines
  -- TODO: Handle code attrs
  Pandoc.Code _ str -> [InlineText $ TextSpan str [CodeMark]]
  -- TODO: Handle other inline elements
  _ -> []

addMark :: Mark -> [InlineSpan] -> [InlineSpan]
addMark mark spans = fmap (addMarkToSpan mark) spans
  where
    addMarkToSpan :: Mark -> InlineSpan -> InlineSpan
    addMarkToSpan m (InlineText textSpan) = InlineText $ TextSpan T.empty [m] <> textSpan
    -- Leave non-text spans (like note refs) unchanged
    addMarkToSpan _ otherSpan = otherSpan

inlineTreeNodeUnfolder :: [InlineSpan] -> (DocNode, [TreeNode])
inlineTreeNodeUnfolder inlineSpans = (TreeNode $ InlineNode inlineSpans, [])
