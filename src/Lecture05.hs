{-# LANGUAGE InstanceSigs #-} -- allows writing type signatures in instances
{-# LANGUAGE DeriveFunctor #-} -- allows deriving Functor where possible

module Lecture5 where

-- import Data.List

{- This lecture covers functor in a bit more detail than our first functor lecture.
 - It first covers a wide variety of functor instances, and then gives some examples
 - of design patterns using the typeclass. -}

-------------------------------
-- Some background on types
--------------------------------

-- Type = Set of possible values

-- Base Types = Bool, Int, Float
-- Algebraic Data Types = [Bool], Tree Int
-- Some "base" types are defined as ADT, some could

-- data Unit = ()
-- data Bool = True | False
-- data Ord = LT | EQ | GT
data Natural = S Natural | Z
data Integer = NonNeg Natural | Neg Natural

-- In both cases, we can talk about the size / cardinality

-- |Bool| = 2
-- |Ord| = 3
-- |Int| = ~ 2^64
-- |Natural| = infinity
-- |Integer| = infinity

-- Why "Algebraic"?

-- data BoolAndOrd = BoolAndOrd Bool Ord
-- |BoolAndOrd| = 2 * 3

-- data BoolOrOrd = ABool Bool | AnOrd Ord
-- |BoolOrOrd| = 2 + 3

-- data BoolToOrd = BoolToOrd (Bool -> Ord)
-- |BoolToOrd| = 3 ^ 2

-- data OrdToBool = OrdToBool (Ord -> Bool)
-- |OrdToBool| = 2 ^ 3


data Sum a b = First a | Second b  -- same as (Either a b)

data Prod a b = Prod a b           -- same as ((,) a b) aka. (a,b)

data Func a b = Func (a -> b)      -- same as (a -> b)

-- > constructors in ghci

-- |Sum a b| = |a| + |b|
-- |Prod a b| = |a| * |b|
-- |Func a b| = |b| ^ |a|

-- |a + b| = |a| + |b|
-- |a * b| = |a| * |b|
-- |a -> b| = |b| ^ |a|


-- data OurType = Con T1 T2 T3 | Con T4 T5
-- |OurType| = (|T1|*|T2|*|T3|) + (|T4|*|T5|)


-- "Algebra"
-- (a + b) * (c + d) = a*c + a*d + b*c + b*d


data LHS a b c d = LHS (Either a b) (Either c d) 
data RHS a b c d = RHS1 a c | RHS2 a d | RHS3 b c | RHS4 b d

isoLR :: LHS a b c d -> RHS a b c d
isoLR (LHS (Left a) (Left c))   = RHS1 a c
isoLR (LHS (Left a) (Right d))  = RHS2 a d
isoLR (LHS (Right b) (Left c))  = RHS3 b c
isoLR (LHS (Right b) (Right d)) = RHS4 b d

isoRL :: RHS a b c d -> LHS a b c d
isoRL (RHS1 a c) = LHS (Left a) (Left c)   
isoRL (RHS2 a d) = LHS (Left a) (Right d)
isoRL (RHS3 b c) = LHS (Right b) (Left c)
isoRL (RHS4 b d) = LHS (Right b) (Right d)

-- isoLR . isoRL == (id :: LHS -> LHS)
-- isoRL . isoRL == (id :: RHS -> RHS)


{-

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-}

-- Review / "Data Structure" Functors

data Identity a = Identity a
  deriving (Show)

data Pair a = Pair a a  -- same as built-in Complex
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


-- TODO give examples as you go

-- > (+ 2) 1
-- > fmap (+ 2) (Identity 1)
-- > (+ 2) <$> (Identity 1)
-- > (+ 2) $ 1

-- > (+ 2) <$> aBlahBlah

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

-- TODO copy down data declarations


instance Functor (Sum a) where
  fmap :: (b -> c) -> Sum a b -> Sum a c
  fmap _ (First x) = First x       -- x :: a
  fmap f (Second x) = Second (f x) -- x :: b

instance Functor (Prod a) where
  fmap :: (b -> c) -> Prod a b -> Prod a c
  fmap f (Prod x y) = Prod x (f y)

instance Functor (Func a) where
  fmap :: (b -> c) -> Func a b -> Func a c
  fmap f (Func g) = Func (f . g)

{- Built-in

instance Functor (Either a) where
  fmap :: (b -> c) -> Either a b -> Either a c
  ...

instance Functor ((,) a) where
  fmap :: (b -> c) -> (a, b) -> (a, c)
  ...

instance Functor ((->) a) where
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

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

{-
readName :: Reader GameState String
readName :: playerName <$> readPlayer

readHealth :: Reader GameState String
readHealth :: playerHealth <$> readPlayer
-}
