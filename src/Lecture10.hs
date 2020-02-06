module Lecture9b where

import Control.Monad as M
import Data.List
import Data.Ratio

-- 1. An example of the list monad

type Pos = (Int, Int)

-- A function that takes a knight's position on an 8 x 8 board
-- and returns all of its next moves
moveKnight :: Pos -> [Pos]
moveKnight (c, r) = filter onBoard
  [(c+2, r-1), (c+2,r+1), (c-2, r-1), (c-2, r+1)
  ,(c+1, r-2), (c+1,r+2), (c-1, r-2), (c-1, r+2)
  ]
  where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]


moveKnight' :: Pos -> [Pos]
moveKnight' (c, r) = do
    (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1)
                ,(c+1, r-2), (c+1, r+2), (c-1, r-2), (c-1, r+2)
                ]
    M.guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

-- moveKnight (6, 2)
-- moveKnight (8, 1)

-- A function that takes a position and returns all the positions that you can reach from in three moves
in3 :: Pos -> [Pos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

-- >>= once: all possible moves from the start
-- >>= second time: for every possible move, every possible next move is computed
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- A function to check whether a we can reach in 3
canReachIn3 :: Pos -> Pos -> Bool
canReachIn3 start end = end `elem` in3 start



-- (6, 2) `canReachIn3` (6, 1)
-- (6, 2) `canReachIn3` (7, 3)

-- Composing monadic functions
-- import Control.Monad
-- (<=<)
-- let f = (+1) . (*100)
-- f 4
-- let g = (\x -> return (x+1)) <=< (\x -> return (x*100))
-- Just 4 >>= g
--
-- What is the type of <=<?
-- Compare to the type (.)

-- First use replicate to mak e alist that contains x compies of the function moveKnight
-- monadically compose all those functions into one
-- let f = foldr (.) id [(+1), (*100), (+1)]
-- f 1
-- We can compose monadic functions in the same way,
-- using <=< instead of normal composition
-- and instead of id we use return

inMany :: Int -> Pos -> [Pos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn x start end = end `elem` inMany x start

-- READER

-- let f = (* 5)
-- let g = (+ 3)
-- (fmap f g) 8
-- 55
{-instance Functor ((->) r) where-}
  {-fmap f g = f . g-}

{--- let f = (+) <$> (*2) <*> (+10)-}
{--- f 3-}
{--- > (+) 6 13-}

{--- f (a -> b) -> f a -> f b-}
{--- f <*> g = \x -> f x (g x)-}
{-instance Applicative ((->) r) where-}
  {-pure x = (\_ -> x)-}
  {-<*> :: (r -> a -> b) -> (r -> a) -> (r -> b)-}
  {-f <*> g = \x -> f x (g x)-}


-- 1) first, to use >>= to feed a monadic value to a function,
-- we get a monadic value.  In this case, we get a function again.
-- 2) all of the implementations isolate the result from the monadic value and apply
-- f to that result.  This is why we do (x r) and then apply f to that.
-- 3) f returns a monadic value, which is a function in our case, so we apply it to r as well.
--
-- Intuitively, using this monad is like passing along an extra argument r to every function.
-- You might use this for configuration

{-instance Monad ((-> r)) where-}
  {-return x = (\_ -> x)-}
  {-(>>=) :: m a -> (a -> m b) -> m b-}
  {-(>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)-}
  {-x >>= f = \r -> f (x r) r-}

  -- (x >>= f) r = f (x r) r
  
{-h >>= f = \w -> f (h w) w-}

-- Do expression always results in a monadic value
-- The result of this value is a function.
-- It takes a number and then (*2) gets applied to that number
-- +10 is applied to that same number and becomes b.
-- a + b 
addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

addStuff' :: Int -> Int
addStuff' x = let
  a = (*2) x
  b = (+10) x
  in a+b

-- https://people.cs.kuleuven.be/~tom.schrijvers/Research/talks/probability_monad.pdf
-- https://doisinkidney.com/posts/2018-06-30-probability-5-ways.html

-- Suppose we have a list [3, 5, 9], 
-- where 3 happens with 0.5 chance
-- 5 happens with 0.25 chance
-- 9 happens with 0.25 chance.
-- We'll use rationals to make sure we don't lose precision.
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

instance Monad Prob where
  return x = Prob [(x, 1%1)]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r))

instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  
  
loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]  

flipThree :: Prob Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c])  


