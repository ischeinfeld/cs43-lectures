module Lecture1 where

import Prelude

---------------------------------
---------------------------------
-- EXPRESSIONS, VALUES, AND TYPES
---------------------------------
---------------------------------

-------------------------------
-- Every expression has a value
-------------------------------

a = True
-- a = False -- immutable

b = c + 1
c = 1 -- order invariant

-- Functions -----------------------------------

inc x = 1 + x

inc' x = (+) 1 x 

-- > inc 4

absMax x y = if abs x > abs y
                then abs x
                else abs y

absMax' x y
    | abs x > abs y = abs x
    | otherwise     = abs y -- otherwise == True

-- > absMax (-5) 2


applyTwice f x = f (f x)

applyTwice' f x = f $ f x

-- > applyTwice inc 4

-- Anonymous Functions --------------------------------

inc'' = \x -> x + 1

absMax'' = \x y -> if abs x > abs y then abs x else abs y

applyTwice'' = \f x -> f (f x)


------------------------------------
-- Every expression/value has a type
------------------------------------

-- > :t True
-- > :t 1 
-- > :t (1 :: Int)
-- > :t (1 :: Float)


-- > :t not
-- > not (1 :: Int)

-- Inference
-- > :t inc 
-- > :t (inc :: Int -> Int)
-- > (inc :: Int -> Int) (1 :: Float)

incInt :: Int -> Int
incInt x = x + 1

-- > incInt (1 :: Integer)


-- Currying ----------------------------

sumInts :: Int -> Int -> Int
sumInts x y = x + y

-- sumInts = \x y -> x + y
-- sumInts = \x -> (y -> x + y)

-- > :t sumInts
-- > :t sumInts 1
-- > sumInts 1 2
-- > (sumInts 1) 2

sumInts' :: Int -> (Int -> Int)
sumInts' = \x -> (\y -> x + y)

-- different semantics for types

incInt' :: Int -> Int          -- read as take and give
incInt' x = sumInts 1 x

incInt'' :: Int -> Int         -- read as value of type (Int -> Int)
incInt'' = \x -> (sumInts 1) x -- applies function to x

incInt''' :: Int -> Int
incInt''' = sumInts 1

sum3 :: Int -> Int -> Int -> Int 
sum3 x y z  = x + y + z



lowestForm :: Int -> Int -> Bool
lowestForm num denom
    | denom == 0         = error "error, divide by 0"
    | gcd num denom /= 1 = False
    | otherwise          = True

-- > :t error
-- pure since expressions always have the same value `when they do`


-----------------------------------
-- Algebraic Datatypes
-----------------------------------

data Person = Student String Int Float
            | Teacher String Int
  deriving (Show)

-- > :t Student
-- > :t Teacher
-- constructors are ordinary functions with extra features ...

-- data Type = Constructor Type Type ...
--           | Constructor 
--           ...
--           | Constructor Type Type ...

-- Constuctors' extra feature, pattern matching

getName :: Person -> String
getName (Student name _ _) = name
getName (Teacher name _ ) = name


-- Another example

data Point = Point Float Float
  deriving (Show)

-- > Point 0 1
-- > :t Point 0 1
-- > :t Point

norm :: Point -> Float
norm (Point x y) = sqrt $ x ^ 2 + y ^ 2

xCoord (Point x _) = x
yCoord (Point _ y) = y


-- Record syntax, only for types with one constructor

data Point' = Point' { xCoord' :: Float,
                       yCoord' :: Float }

