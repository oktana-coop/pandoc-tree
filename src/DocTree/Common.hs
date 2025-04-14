{-# LANGUAGE InstanceSigs #-}

module DocTree.Common (BlockNode (..), TextSpan (..), Mark (..), LinkMark (..)) where

import qualified Data.Text as T
import Text.Pandoc.Definition as Pandoc (Attr, Block (..), Target)

data BlockNode = PandocBlock Pandoc.Block | ListItem [Pandoc.Block] deriving (Show, Eq)

data LinkMark = Link Pandoc.Attr Pandoc.Target deriving (Show, Eq)

instance Ord LinkMark where
  compare :: LinkMark -> LinkMark -> Ordering
  compare (Link _ target1) (Link _ target2) = compare target1 target2

data Mark = EmphMark | StrongMark | LinkMark LinkMark deriving (Show, Eq, Ord)

data TextSpan = TextSpan {value :: T.Text, marks :: [Mark]} deriving (Show, Eq)

instance Semigroup TextSpan where
  (<>) (TextSpan value1 marks1) (TextSpan value2 marks2) = TextSpan (value1 <> value2) (marks1 <> marks2)

instance Monoid TextSpan where
  mempty = TextSpan T.empty []
