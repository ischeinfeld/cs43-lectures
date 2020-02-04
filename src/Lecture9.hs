{-# LANGUAGE InstanceSigs #-} -- allows writing type signatures in instances
{-# LANGUAGE DeriveFunctor #-} -- allows deriving Functor where possible


module Lecture9 where
import Control.Applicative

{--


class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
  return :: a -> m a
  >>= :: m a -> (a -> m b) -> m b
--}
-- returning to MyList
newtype MyList a = MyList [a]
  deriving (Show)

instance Functor MyList where
  fmap f (MyList l) = MyList (fmap f l)

instance Applicative MyList where
  pure val = MyList [val]
  -- (<*>) :: MyList (a -> b) -> MyList a -> MyList b
  (MyList fs) <*> (MyList xs) = MyList (fs <*> xs)

  -- defining Functor and Applicative using the [] instances

-- useful helper
unpack :: MyList a -> [a]
unpack (MyList l) = l
-- somewhat contrived method. Exists to concat a MyList of MyLists into a single MyList
concatMyList :: MyList (MyList a) -> MyList a
concatMyList (MyList l) = MyList (concat (fmap unpack l))

-- now we write a very sensible monad instance for MyList
instance Monad MyList where
  return = pure
  ml >>= f = (concatMyList (fmap f ml))

-- the builtin list works exactly the same.
-- some examples.


-- writing bind for maybe
bindMaybe :: (Maybe a) -> (a -> Maybe b) -> (Maybe b)
bindMaybe (Just x) f = f x
bindMaybe _ f = Nothing

-- >>= is written the same way. Try it out.

-- an object-oriented example, and do notation.
-- representing that a student has enrolled at a university this term
data Enrollment = Enrollment {
studentName :: String,
studentId :: Int,
totalUnits :: Int
} deriving (Show)

checkStudentName :: String -> Maybe String
checkStudentName x
  |  (length x < 100) = Just x
  |  otherwise = Nothing

checkStudentId :: Int -> Maybe Int
checkStudentId x
  |  ((x >= 0) && (x <= 2^24)) = Just x
  |  otherwise = Nothing


checkTotalUnits :: Int -> Maybe Int
checkTotalUnits x
  |  ((x >= 12) && (x <= 20)) = Just x
  |  otherwise = Nothing

safeEnroll :: String -> Int -> Int -> Maybe Enrollment
safeEnroll studentName studentId totalUnits =
  case (checkStudentName studentName) of
    Nothing -> Nothing
    Just name ->

      case (checkStudentId studentId) of
        Nothing -> Nothing
        Just sid ->

          case (checkTotalUnits totalUnits) of
              Nothing -> Nothing
              Just units -> Just (Enrollment name sid units)

safeEnroll2 :: String -> Int -> Int -> Maybe Enrollment
safeEnroll2 studentName studentId totalUnits =
  (checkStudentName studentName) >>= (\name ->
    (checkStudentId studentId) >>= (\sid ->
      (checkTotalUnits totalUnits) >>= (\units ->
        Just (Enrollment name sid units)
      )
    )
  )

safeEnroll3 :: String -> Int -> Int -> Maybe Enrollment
safeEnroll3 studentName studentId totalUnits = do
  name <- checkStudentName studentName
  sid <- checkStudentId studentId
  units <- checkTotalUnits totalUnits
  return (Enrollment name sid units)
