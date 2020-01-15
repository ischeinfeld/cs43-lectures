module Lecture3 where

data List a = Empty | Cons a (List a)

bools = Cons True $ Cons False Empty

ints :: List Int
ints = Cons 1 $ Cons 2 $ Cons 3 Empty
names = Cons "Kylan" $ Cons "Pablo" Empty

mapList _ Empty = Empty
mapList f (Cons x xs) = Cons (f x) $ mapList f xs

filterList _ Empty = Empty
filterList p (Cons x xs)
  | p x = Cons x $ filterList p xs
  | otherwise = filterList p xs


sumIntList :: List Int -> Int
sumIntList = foldList (+) 0

prodIntList :: List Int -> Int
prodIntList = foldList (*) 1

foldList f acc Empty = acc
foldList f acc (Cons x xs) = f x $ foldList f acc xs 


{-

class Show a where
  show :: a -> String

-}

instance Show a => Show (List a) where
  show Empty = "[]"
  show (Cons x xs) = "[" ++ show x ++ showVals xs ++ "]"
    where showVals xs = foldList (\x str -> "," ++ show x ++ str) "" xs

{-

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x /= y = not (x == y)
  x == y = not (x /= y)

-}
