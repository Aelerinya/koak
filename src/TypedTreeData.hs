module TypedTreeData (TypedTree (TypedNode)) where

data TypedTree = TypedNode String String [TypedTree] deriving (Show, Eq)
