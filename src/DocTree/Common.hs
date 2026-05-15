{-# LANGUAGE InstanceSigs #-}

module DocTree.Common (BlockNode (..), TextSpan (..), Mark (..), LinkMark (..), InlineSpan (..), Image (..), NoteId (..)) where

import qualified Data.Text as T
import Text.Pandoc.Definition as Pandoc (Attr, Block (..), Caption, Inline, Target)

newtype NoteId = NoteId T.Text deriving (Show, Eq, Ord)

data BlockNode = PandocBlock Pandoc.Block | ListItem [Pandoc.Block] | NoteContent NoteId [Pandoc.Block] | Caption Pandoc.Caption | FigureContent [Pandoc.Block] deriving (Show, Eq)

data LinkMark = Link Pandoc.Attr Pandoc.Target deriving (Show, Eq)

instance Ord LinkMark where
  compare :: LinkMark -> LinkMark -> Ordering
  compare (Link _ target1) (Link _ target2) = compare target1 target2

data Mark = EmphMark | StrongMark | LinkMark LinkMark | CodeMark deriving (Show, Eq, Ord)

data TextSpan = TextSpan {value :: T.Text, marks :: [Mark]} deriving (Show, Eq, Ord)

data Image = Image Pandoc.Attr [Pandoc.Inline] Pandoc.Target deriving (Show, Eq)

instance Ord Image where
  compare :: Image -> Image -> Ordering
  compare (Image _ _ target1) (Image _ _ target2) = compare target1 target2

data InlineSpan = InlineText TextSpan | NoteRef NoteId | InlineImage Image deriving (Show, Eq, Ord)

instance Semigroup TextSpan where
  (<>) (TextSpan value1 marks1) (TextSpan value2 marks2) = TextSpan (value1 <> value2) (marks1 <> marks2)

instance Monoid TextSpan where
  mempty = TextSpan T.empty []
