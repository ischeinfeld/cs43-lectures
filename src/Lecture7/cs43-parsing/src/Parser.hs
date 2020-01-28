{-# LANGUAGE LambdaCase #-}   -- allows syntactic sugar for case in lambdas
{-# LANGUAGE InstanceSigs #-} -- allows type signatures in instances

{- Set up
 - $ stack new cs43-parsing-demo hspec
 - $ tree
 - walk through cabal
 - switch to cs43-parsing
 - $ tree
 - walk through cabal
 -}

module Parser where

import Data.Char 
import Control.Applicative

{-
 - Applicative:
 -
 - class Functor f => Applicative f where
 -   pure :: a -> f a
 -   (<*>) :: f (a -> b) -> f a -> f b
 -
 - > (+) <$> (Just 4) <*> (Just 3)
 -
 - Show docs, discuss liftA2, <*, *>
 - 
 - Show Maybe instance for Applicative in docs
 -
 - > liftA2 (+) (Just 4) (Just 3)
 - > (Just 4) *> (Just 3)
 - > (Just 4) <* (Just 3)
 -
 - Show List instance for Applicative in docs
 -
 - class Applicative f => Alternative f where
 -   empty :: f a
 -   (<|>) :: f a -> f a -> f a
 -
 - Show docs, loosely explain some and many
 -
 - Show List and Maybe instance
 -}

newtype Parser a = P { getParser :: (String -> Maybe (a,String)) }

-- note Parser a is a parser that might give a value of type a

-- captures first character, fails on empty string
item :: Parser Char
item = P $ \case 
  []      -> Nothing
  (x:xs)  -> Just (x,xs)

-- > getParser item "hello"
-- > getParser item ""

instance Functor Parser where
  fmap f p = P $ \inp -> case getParser p inp of
    Nothing       -> Nothing
    Just (v, out) -> Just (f v, out)

-- > getParser (toUpper <$> item) "hello"

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \inp -> Just (x, inp)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \inp -> case getParser pf inp of
    Nothing -> Nothing
    Just (f, tmp) -> getParser (fmap f px) tmp

-- > :t (,) <$> item <*> item
-- > getParser ((,) <$> item <*> item) "hello"
-- > getParser (item *> item) "hello"
-- > getParser (item <* item) "hello"



-- we can write functions that generate parameterized parsers

-- parser that captures character if it satisfies a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = P $ \case
  x:xs | pred x -> Just (x,xs)
  _ -> Nothing

-- and we can use these to write new parsers

-- parser that captures a specific character
char :: Char -> Parser Char
char = satisfy . (==)  -- char c = satisfy (== c)

notChar :: Char -> Parser Char
notChar = satisfy . (/=)

-- > getParser (char 'h') "hello"
-- > getParser (char 'g') "hello"

-- parser that captures any digit
digit :: Parser Char
digit = satisfy isDigit


-- add one more typeclass instance

instance Alternative Parser where
  empty :: Parser a -- represents a failed parser, identity under <|>
  empty = P $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \inp -> case getParser p inp of
    Nothing -> getParser q inp -- run second parser if first fails
    result -> result             -- if first succeeds, ignore second

-- > getParser (char 'h' <|> char 'g') "hello"
-- > getParser (char 'h' <|> char 'g') "gello"
-- > getParser (char 'h' <|> char 'g') "jello"

-- parser that captures >= 0 spaces
spaces :: Parser String
spaces = many $ char ' '

-- > getParser spaces "     hello"
-- > getParser (spaces *> item) "     hello"
