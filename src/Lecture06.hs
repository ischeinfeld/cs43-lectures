{-# LANGUAGE InstanceSigs #-} -- allows writing type signatures in instances
{-# LANGUAGE DeriveFunctor #-} -- allows deriving Functor where possible


module Lecture6 where
import Control.Applicative

-- review of fmap
r = fmap (+1) (Just 5)
q = funcMaybe (Just (+1)) (Just 2)

-- funcMaybe
funcMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
funcMaybe (Just f) (Just x) = Just (f x)
funcMaybe _ _ = Nothing




-- introducing... Applicative
{--
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
--}
{--
instance Applicative Maybe where
  pure = Just
  (<*>) = funcMaybe
--}

-- Writing the standard [] applicative

newtype MyList a = MyList [a]
  deriving (Show)

instance Functor MyList where
  fmap f (MyList l) = MyList (fmap f l)

instance Applicative MyList where
  pure val = MyList [val]
  -- (<*>) :: MyList (a -> b) -> MyList a -> MyList b
  (MyList (f:fs)) <*> (MyList l) = MyList ((fmap f l) ++ rest) where
    (MyList rest) = (MyList fs) <*> (MyList l)
  --(MyList fs) <*> (MyList xs) = MyList [f x|  f <- fs, x <- xs]

double :: Int -> Int
double = (*2)

minus10 :: Int -> Int
minus10 x = x - 10

coolList :: [Int]
coolList = [1,2,3,4,5]

-- The ZipList [] applicative
--see: zipWith

newtype MyZipList a = MyZipList [a]
  deriving (Show)

instance Functor MyZipList where
  fmap f (MyZipList l) = MyZipList (fmap f l)

instance Applicative MyZipList where
  pure x = MyZipList (repeat x)
  (MyZipList fs) <*> (MyZipList xs) = MyZipList (zipWith ($) fs xs)


-- Person
data Person = Person { name :: String, age :: Int}
  deriving (Show)

maybeConstructor :: Maybe String -> Maybe Int -> Maybe Person
maybeConstructor maybeName maybeAge = (pure Person) <*> maybeName <*> maybeAge

-- liftA2. was not included in lecture but it is defined as follows.
-- feel free to look up on hoogle, though you can certain do assignments without knowing it
liftA2 :: (a -> b -> c) -> f a -> f b -> f c
liftA2 g a b c = pure g <*> a <*> b <*> c
