module Lecture2 where

--- OptionalFloat
data OptionalFloat = FloatExists Float | FloatDoesnt
  deriving (Show)

mySqrt :: Float -> Float
mySqrt x = mySqrtRec x 0 x 0.0001
mySqrtRec :: Float -> Float -> Float -> Float -> Float
mySqrtRec x lo hi epsilon
  | (hi - lo) < epsilon = mid
  | mid * mid > x = mySqrtRec x lo mid epsilon
  | otherwise = mySqrtRec x mid hi epsilon
  where mid = (lo + hi) / 2

mySqrtSafer :: Float -> OptionalFloat
mySqrtSafer x
  | x < 0 = FloatDoesnt
  | otherwise = FloatExists (mySqrt x)

-- Optional
data Optional a = Exists a | Doesnt
  deriving (Show)

mySqrtSafer' :: Float -> Optional Float
mySqrtSafer' x
  | x < 0 = Doesnt
  | otherwise = Exists (mySqrt x)

-- Equivalent built-in type Maybe a, defined as
--   data Maybe a = Just a | Nothing

data EitherStringOrInt = Reason String | Result Int
  deriving (Show)

superSafeDivide :: Int -> Int -> EitherStringOrInt
superSafeDivide a b
  | b == 0 = Reason "cannot divide by 0"
  | a `rem` b /= 0 = Reason $ (show a) ++ " is not divisible by "++(show b)
  | otherwise = Result $ div a b


-- data Either a b = Left a | Right b

superSafeDivide' :: Int -> Int -> Either String Int
superSafeDivide' a b
  | b == 0 = Left "cannot divide by 0"
  | a `rem` b /= 0 = Left $ (show a) ++ " is not divisible by "++(show b)
  | otherwise = Right $ div a b


-- Recursive datatypes

-- Note (List a), not List, specified as the type of the second argument
-- to the PushFront constructor, since List is a type constructor and 
-- (List a) a fully realized type.
data List a = Empty | PushFront a (List a) 
  deriving (Show)

three_things = PushFront (1::Int) $ PushFront 4 $ PushFront 2 $ Empty

headList :: List a -> Maybe a
headList (PushFront i is) = Just i
headList Empty = Nothing

lengthList :: List a -> Int
lengthList Empty = 0
lengthList (PushFront i is) = 1 + lengthList is

filterList :: (a -> Bool) -> List a -> List a
filterList predicate Empty = Empty
filterList predicate (PushFront x xs)
  | predicate x = PushFront x (filterList predicate xs)
  | otherwise = filterList predicate xs

isEven x = rem x 2 == 0
listToFilter = PushFront 1 $ PushFront 2 $ PushFront (-1) $ Empty

mapList :: (a -> b) -> List a -> List b
mapList f Empty = Empty
mapList f (PushFront x xs) = PushFront (f x) (mapList f xs)

double = (*) 2
listToMap = PushFront 2 $ PushFront 140 $ Empty

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
  where lesser = filter (<x) xs
        greater = filter (>=x) xs
