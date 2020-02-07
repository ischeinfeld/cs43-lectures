module Lecture10 where

import Control.Monad
import Data.Ratio

-- Functor :   (a -> b) -> (f a -> f b) lifts function
-- Appl... : f (a -> b) -> (f a -> f b) lifts functor of function
-- Monad   : (a -> f b) -> (f a -> f b) lifts function returning functor


{- Functor f => Applicative f => Monad f

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
  (>>=) :: f a -> (a -> f b) -> f b

-- Also have the following functions in Monad, which are implemented
-- in Applicative

  return :: a -> f a
  return = pure

  (>>) :: f a -> f b -> f b
  (>>) = (*>) = liftA2 (flip const) = \x y -> x >>= \_ -> y

  -- note Monad "is-a" Applicative relationship, let's you use >> or *>
  -- note recursive bindings for *>, >>

-}

-- > fmap (+ 5) (Just 6)
-- > fmap (+ 5) Nothing

-- > (Just (+ 5)) <*> (Just 6)
-- > Nothing <*> (Just 6)

-- > (\x -> if x > 0 then Just x else Nothing) =<< (Just 5)
-- > (\x -> if x > 0 then Just x else Nothing) =<< Nothing
-- > (\x -> if x > 0 then Just x else Nothing) =<< (Just (-5))

thing1 = do
  Just 5
  Just 6

thing2 = do
  x <- Just 5
  return x

thing3 = do
  (x, y) <- Just (5, 6)
  return x

thing4 = do
  let x = 5
  Just x


type Pos = (Int, Int)

moveKnight :: Pos -> [Pos]
moveKnight (c, r) = filter onBoard
  [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1)
  ,(c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)
  ]
  where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: Pos -> [Pos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

in3' start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: Pos -> Pos -> Bool
canReachIn3 start end = end `elem` in3 start

inMany :: Int -> Pos -> [Pos]
inMany n start = return start >>= foldr (<=<) return (replicate n moveKnight)

canReachIn n start end = end `elem` inMany n start

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
  fmap f xs = Prob [ (f x, p) | (x, p) <- getProb xs ]

instance Applicative Prob where
  pure x = Prob [(x, 1)]
  fs <*> xs
    = Prob
    [ (f x, fp * xp)
    | (f, fp) <- getProb fs
    , (x, xp) <- getProb xs ]

instance Monad Prob where
  return = pure
  -- (>>=) :: Prob a -> (a -> Prob b) -> Prob b
  xs >>= f
    = Prob
    [ (y, xp*yp)
    | (x, xp) <- getProb xs
    , (y, yp) <- getProb (f x)]

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipTwo = do
  a <- coin
  b <- loadedCoin
  return [a, b]

allTails :: Prob Bool
allTails = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (==Tails) [a, b, c])
