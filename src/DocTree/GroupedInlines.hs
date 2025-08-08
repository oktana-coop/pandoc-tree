module DocTree.GroupedInlines (BlockNode (..), InlineSpan (..), DocNode (..), TreeNode (..), toTree) where

import Control.Monad.State (State, get, modify, runState)
import qualified Data.Text as T
import Data.Tree (Tree (Node), unfoldForestM, unfoldTreeM)
import DocTree.Common (BlockNode (..), InlineSpan (..), LinkMark (..), Mark (..), NoteId (..), TextSpan (..))
import Text.Pandoc.Definition as Pandoc (Block (..), Inline (..), Pandoc (..))

data InlineNode = InlineContent [InlineSpan] deriving (Show, Eq)

data TreeNode = BlockNode BlockNode | InlineNode InlineNode deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)

data NoteData = NoteData
  { noteCounter :: Int,
    -- Accumulated note content block nodes
    noteContents :: [Tree DocNode]
  }

type NotesState = State NoteData

toTree :: Pandoc.Pandoc -> Tree DocNode
toTree (Pandoc.Pandoc _ blocks) = Node Root forestWithNotes
  where
    blockNodes = map (BlockNode . PandocBlock) blocks
    (mainForest, notesState) = runState (unfoldForestM treeNodeUnfolder blockNodes) initialState
    initialState = NoteData 0 []
    noteTrees = noteContents notesState
    forestWithNotes = mainForest <> noteTrees

treeNodeUnfolder :: TreeNode -> NotesState (DocNode, [TreeNode])
treeNodeUnfolder (BlockNode blockNode) = blockTreeNodeUnfolder blockNode
treeNodeUnfolder (InlineNode inlineNode) = return $ inlineTreeNodeUnfolder inlineNode

blockTreeNodeUnfolder :: BlockNode -> NotesState (DocNode, [TreeNode])
blockTreeNodeUnfolder (PandocBlock block) = case block of
  Pandoc.Plain inlines -> do
    inlineNode <- buildInlineNode inlines
    return ((TreeNode . BlockNode . PandocBlock . Pandoc.Plain) [], [inlineNode])
  Pandoc.Para inlines -> do
    inlineNode <- buildInlineNode inlines
    return ((TreeNode . BlockNode . PandocBlock . Pandoc.Para) [], [inlineNode])
  Pandoc.Header level attrs inlines -> do
    inlineNode <- buildInlineNode inlines
    return (TreeNode . BlockNode $ PandocBlock $ Pandoc.Header level attrs [], [inlineNode])
  Pandoc.CodeBlock attrs text -> return (TreeNode . BlockNode $ PandocBlock $ Pandoc.CodeBlock attrs T.empty, [InlineNode $ InlineContent $ [InlineText $ TextSpan {value = text, marks = []}]])
  Pandoc.BulletList items -> return ((TreeNode . BlockNode . PandocBlock . Pandoc.BulletList) [], map (BlockNode . ListItem) items)
  Pandoc.OrderedList attrs items -> return (TreeNode $ BlockNode $ PandocBlock $ Pandoc.OrderedList attrs [], map (BlockNode . ListItem) items)
  Pandoc.BlockQuote children -> return ((TreeNode . BlockNode . PandocBlock . Pandoc.BlockQuote) [], map (BlockNode . PandocBlock) children)
  _ -> undefined
blockTreeNodeUnfolder (ListItem children) = return ((TreeNode . BlockNode . ListItem) [], map (BlockNode . PandocBlock) children)
blockTreeNodeUnfolder (NoteContent noteId children) = return (TreeNode $ BlockNode $ NoteContent noteId [], map (BlockNode . PandocBlock) children)

buildInlineNode :: [Pandoc.Inline] -> NotesState TreeNode
buildInlineNode inlines = fmap (InlineNode . InlineContent) $ pandocInlinesToSpans inlines

pandocInlinesToSpans :: [Pandoc.Inline] -> NotesState [InlineSpan]
pandocInlinesToSpans inlines =
  -- Use fmap to lift `mergeSameMarkSpans . concat` over the State structure
  fmap (mergeSameMarkSpans . concat) (perInlineSpans inlines)
  where
    -- Convert each inline into a list of spans, inside the State monad.
    perInlineSpans :: [Pandoc.Inline] -> NotesState [[InlineSpan]]
    perInlineSpans = mapM inlineToSpans

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

inlineToSpans :: Pandoc.Inline -> NotesState [InlineSpan]
inlineToSpans inline = case inline of
  Pandoc.Str str -> return [InlineText $ TextSpan str []]
  Pandoc.Space -> return [InlineText $ TextSpan (T.pack " ") []]
  Pandoc.Strong inlines -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark StrongMark wrappedSpans
  Pandoc.Emph inlines -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark EmphMark wrappedSpans
  Pandoc.Link attrs inlines target -> do
    wrappedSpans <- pandocInlinesToSpans inlines
    return $ addMark (LinkMark $ DocTree.Common.Link attrs target) wrappedSpans
  -- TODO: Handle code attrs
  Pandoc.Code _ str -> return [InlineText $ TextSpan str [CodeMark]]
  Pandoc.Note noteBlocks -> do
    -- Generate note ID and create note content
    notesState <- get
    let newNoteId = noteCounter notesState + 1
        noteIdText = T.pack $ show newNoteId
        noteId = NoteId noteIdText

    -- Convert note blocks to spans
    noteContentTree <- unfoldTreeM treeNodeUnfolder (BlockNode $ NoteContent noteId noteBlocks)

    -- Update state
    modify
      ( \currentNotestState ->
          -- Getting a new state using the record update syntax.
          currentNotestState
            { noteCounter = newNoteId,
              noteContents = noteContents currentNotestState <> [noteContentTree]
            }
      )

    -- Return note ref node
    return [NoteRef noteId]
  -- TODO: Handle other inline elements
  _ -> return []

addMark :: Mark -> [InlineSpan] -> [InlineSpan]
addMark mark spans = fmap (addMarkToSpan mark) spans
  where
    addMarkToSpan :: Mark -> InlineSpan -> InlineSpan
    addMarkToSpan m (InlineText textSpan) = InlineText $ TextSpan T.empty [m] <> textSpan
    -- Leave non-text spans (like note refs) unchanged
    addMarkToSpan _ otherSpan = otherSpan

inlineTreeNodeUnfolder :: InlineNode -> (DocNode, [TreeNode])
inlineTreeNodeUnfolder inlineNode = (TreeNode $ InlineNode inlineNode, [])
