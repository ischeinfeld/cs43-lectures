module Lecture9b where

import Control.Monad as M
import Data.List

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

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

-- https://people.cs.kuleuven.be/~tom.schrijvers/Research/talks/probability_monad.pdf
