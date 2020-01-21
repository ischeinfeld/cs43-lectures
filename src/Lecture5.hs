{-# LANGUAGE InstanceSigs #-} -- allows writing type signatures in instances
{-# LANGUAGE DeriveFunctor #-} -- allows deriving Functor where possible

module Lecture5 where

-- import Data.List

{- This lecture covers functor in a bit more detail than our first functor lecture.
 - It first covers a wide variety of functor instances, and then gives some examples
 - of design patterns using the typeclass. -}

-------------------------------
-- Give some background on types
--------------------------------

-- Type = Set of possible values
-- |Type| = Size of set
-- data And = And T1 T2
-- |And| = |T1| x |T2|
-- And = T1 x T2

-- data Or = Left T1 | Right T2
-- |Or| = |T1| + |T2|
-- Or = T1 + T2

-- Algebraic Datatypes
-- data OurType = Con T1 T2 T3 | Con T4 T5
-- |OurType| = (|T1|*|T2|*|T3|) + (|T4|*|T5|)

{-

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-}

-- Review / "Data Structure" Functors

data Identity a = Identity a
  deriving (Show)

data Pair a = Pair a a  -- same as built-in (,) or Complex
  deriving (Show)

data Optional a = Missing | Existing a  -- same as built-in Maybe
  deriving (Show)

data List a = Empty | Cons a (List a)
  deriving (Show)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

data RoseTree a = RoseTree a [RoseTree a]
  deriving (Show)


aList = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Empty :: List Int
aTree = Node (Node Leaf 2 Leaf) 1 Leaf :: Tree Int
aRoseTree = RoseTree 1 [RoseTree 2 [], RoseTree 3 [RoseTree 4 []]] :: RoseTree Int


instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Functor List where
  fmap f Empty = Empty
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)

instance Functor RoseTree where
  fmap f (RoseTree x trees) = RoseTree (f x) $ map (fmap f) trees


-- > (+ 2) 1
-- > fmap (+ 2) (Identity 1)
-- > (+ 2) <$> (Identity 1)
-- > (+ 2) $ 1

-- > (+ 2) <$> aBlahBlah

-- > :info Functor
-- > :t (<$)
-- > 0 <$ aBlahBlah
-- How would you write this, if it wasn't built-in?
-- > fmap (\_ -> 0) aBlahBlah
-- HOOGLE: a -> b -> a
-- > :t const
-- > :t const (0 :: Int)
-- > fmap (const 0) aBlahBlah
-- > (fmap (const 0)) aBlahBlah
-- > (fmap . const) 0 aBlahBlah


{-

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  
  (<$) :: a -> f b -> f a
  (<$) = fmap . const  -- default implementation

-}

-- Functor for types of kind * -> * -> *

data SumType a b = First a | Second b
  deriving (Show)

data ProductType a b = Product a b
  deriving (Show)

data FunctionType a b = Function (a -> b)



instance Functor (SumType a) where
  fmap :: (b -> c) -> SumType a b -> SumType a c
  fmap _ (First x) = First x       -- x :: a
  fmap f (Second x) = Second (f x) -- x :: b

instance Functor (ProductType a) where
  fmap :: (b -> c) -> ProductType a b -> ProductType a c
  fmap f (Product x y) = Product x (f y)

instance Functor (FunctionType a) where
  fmap :: (b -> c) -> FunctionType a b -> FunctionType a c
  fmap f (Function g) = Function (f . g)

{- Built-in

instance Functor ((->) a) where
  fmap :: (b -> c) -> (->) a b -> (->) a c
  fmap = (.) -- expand type above to show

-}


{- Rewrite in record sytax
 
data Reader e a = Reader (e -> a)

runReader :: Reader e a -> (e -> a)
runReader Reader f = f

-}

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
  fmap :: (a -> b) -> Reader e a -> Reader e b
  fmap f (Reader g) = Reader $ f . g

-- example showing Reader and refactoring

type ID = Int

data Tile = Wall | Path | Forest

data Player = Player { playerName :: String
                     , playerHealth :: Float }

data GameState = GameState { player :: Player
                           , level :: Int }

readPlayer :: Reader GameState Player
readPlayer = Reader player

readLevel :: Reader GameState Int
readLevel = Reader level

readName :: Reader GameState String
readName :: playerName <$> readPlayer

readHealth :: Reader GameState String
readHealth :: playerHealth <$> readPlayer

