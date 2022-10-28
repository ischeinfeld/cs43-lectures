{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Lecture16Background where

import qualified Database.Redis as Redis

-- THING 1, Type Families

-- So far we've seen things like

-- > :t True
-- > :k Bool

-- > :t Just
-- > :k Maybe -- higher kinded type

-- DataKinds turns types (type constructors) with values (value constructors) into 
--   kinds (kind constructors) with types (type constructors)
--
-- > :set -XDataKinds
-- > :t True
-- > :k 'True
--
-- > :t Just
-- > :k 'Just
-- > :t Nothing
-- > :k 'Nothing

import GHC.TypeLits

-- > :t 1
-- > :k 1


type family If c t e where
  If 'True  t e = t
  If 'False t e = e

-- > :kind If 'True Char Int
-- > :kind! If 'True Char Int
-- > :kind If


-- just like Bool and Maybe are promoted by DataKinds, so is []

-- before, [] type constructor, [] and : term constructors
-- now   , [] kind constructor, '[] and ': type constructors


-- > :t []
-- > :t (:)

-- > :kind '[]
-- > :kind '(:)

-- > :kind [] 

-- > :type [True, False]
-- > :kind [True, False]

-- > :type [1, 2]
-- > :kind [1, 2]

type family Length xs where -- (xs :: [k]) 
  Length '[]       = 0
  Length (x ': xs) = 1 + Length xs

-- > :kind (+)

-- > :kind! Length [Char,Bool,Int]
-- > :kind! Length [1,2,3]



-- THING 2, Free Monads

data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free x) = Free (fmap f <$> x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Free x = Free (fmap f <$> x)
  Free x <*> my     = Free ((<*> my) <$> x)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free x >>= f = Free ((>>= f) <$> x)

infixr 0 ~>
type f ~> g = forall x. f x -> g x

-- > :k (->)
-- > :k (~>)


freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM phi (Pure x) = Pure x
freeM phi (Free fx) = Free $ phi (freeM phi <$> fx)


monad :: Monad m => Free m ~> m
monad (Pure x) = pure x
monad (Free mfx) = do
  fx <- mfx
  monad fx

interp :: (Functor f, Monad m) => f ~> m -> Free f ~> m
interp phi = monad . freeM phi


data Sum f g a = InL (f a) | InR (g a)

sumNat :: (f ~> t) -> (g ~> t) -> (Sum f g) ~> t
sumNat phi _   (InL x) = phi x
sumNat _   psi (InR x) = psi x

-- | Key value store functionality.
data KeyValF a
  = GetKey String (Maybe String -> a)
  | PutKey String String a
  deriving (Functor)

-- | Console functionality.
data ConsoleF a
  = PutStrLn String a
  | GetLine (String -> a)
  deriving (Functor)


{-
-- Console in IO:
consoleIO :: ConsoleF ~> IO
consoleIO (PutStrLn s v) = do
  Prelude.putStrLn s
  pure v
consoleIO (GetLine cb) = do
  s <- Prelude.getLine
  pure (cb s)


-- KeyValue in IO via Redis.
keyValIO :: KeyValF ~> IO
keyValIO (GetKey k cb) = do
  r <- Redis.lookupKey k
  pure (cb r)
keyValIO (PutKey k v n) = do
  Redis.putKeyVal k v
  pure n
-}
