module TreeData (Tree (Empty, Node)) where

data Tree = Empty | Node String [Tree] deriving (Show, Eq)
