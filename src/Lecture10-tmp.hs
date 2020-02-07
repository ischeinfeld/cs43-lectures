module Lecture10 where

import Control.Monad as M
import Data.List
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
  Just 5       -- Just 5 >> 
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

-- A function that takes a knight's position on an 8 x 8 board
-- and returns all of its next moves

moveKnight :: Pos -> [Pos]
moveKnight (c, r) = filter onBoard
  [(c+2, r-1), (c+2,r+1), (c-2, r-1), (c-2, r+1)
  ,(c+1, r-2), (c+1,r+2), (c-1, r-2), (c-1, r+2)
  ]
  where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]

-- moveKnight (6, 2)
-- moveKnight (8, 1)

in3 :: Pos -> [Pos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: Pos -> Pos -> Bool
canReachIn3 start end = end `elem` in3 start

-- (6, 2) `canReachIn3` (6, 1)
-- (6, 2) `canReachIn3` (7, 3)

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
inMany n start = return start >>= foldr (<=<) return (replicate n moveKnight)

canReachIn x start end = end `elem` inMany x start

--

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
  xs >>= f
    = Prob
    [ (y, xp*yp)
    | (x, xp) <- getProb xs
    , (y, yp) <- getProb (f x)]

data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  
  
loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]  

flipTwo = do
  a <- coin
  b <- loadedCoin
  return [a, b]

allTails :: Prob Bool  
allTails = do  
    a <- coin
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c])

--
-- READER

-- let f = (* 5)
-- let g = (+ 3)
-- (fmap f g) 8
-- 55

{-instance Functor ((->) r) where -}
  {-fmap :: (a -> b) -> f a -> f b-}
  {-fmap :: (a -> b) -> (r -> a) -> (r -> b)-}
  {-fmap f g = f . g-}

-- let f = (+) <$> (*2) <*> (+10)
-- f 3
-- > (+) 6 13

-- f (a -> b) -> f a -> f b
-- f <*> g = \x -> f x (g x)
instance Applicative ((->) r) where
  pure x = (\_ -> x)
  <*> :: (r -> a -> b) -> (r -> a) -> (r -> b)
  f <*> g = \x -> f x (g x)

instance Monad ((-> r)) where
  return x = (\_ -> x)
  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  x >>= f = \r -> f (x r) r

  -- (x >>= f) r = f (x r) r
  
{-h >>= f = \w -> f (h w) w-}

-- Do expression always results in a monadic value
-- The result of this value is a function.
-- It takes a number and then (*2) gets applied to that number
-- +10 is applied to that same number and becomes b.
-- a + b 

addStuff :: Int -> Int
addStuff = do
  a <- (*2) -- (* 2) >>= \a ->
  b <- (+10) -- (+ 10) >>= \b -> 
  return (a+b) -- return (a+b)

addStuff' = (*2) >>= (+)

addStuff :: Int -> Int
addStuff = (\r -> r * 2) >>= (\x ->
           (\r -> r + 10) >>= (\y ->
           return (x + y)))

