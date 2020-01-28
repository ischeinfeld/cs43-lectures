module Lecture4 where

  -- kind

  -- Maybe
  -- Either
  -- Either a
  -- Either (Maybe Int)
  -- (->)

-- data Maybe a = Just a | Nothing

-- data Either a b = Left a | Right b

class PutThreeTogether l where
  putThem :: a -> a -> a -> (l a)

instance PutThreeTogether [] where
  putThem x y z = [x,y,z]

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance PutThreeTogether Tree where
  putThem a b c = Node (Node Leaf b Leaf) a (Node Leaf c Leaf)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Leaf = Leaf
treeMap f (Node left a right) = Node (treeMap f left) (f a) (treeMap f right)

someTree = Node (Node Leaf 2 Leaf) 3 (Node Leaf 1 Leaf) :: Tree Int

instance Functor Tree where
  fmap = treeMap

mapDoubleTo :: Functor f => f Int -> f Int
mapDoubleTo structure = fmap (*2) structure


  -- irrelevant
  --class Functor f where
  --  fmap :: (a -> b) -> f a -> f b
  -- Hoogle + Functor laws

  -- Maybe and []

  -- (fmap . fmap)
  -- type?

  -- fmap on Either, tuple, Constant

data Constant a b =
  Constant {getConstant :: a}
