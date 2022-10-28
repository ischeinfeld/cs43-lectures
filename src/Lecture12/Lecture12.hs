{-# Language InstanceSigs #-}

module Lecture12 where

import Data.Functor.Identity
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.Trans.Class

-- data Identity a = Identity a
-- data Maybe a = Just a | Nothing
-- data Reader e a = Reader (e -> a)

newtype IdentityT m a = 
  IdentityT { runIdentityT :: m (Identity a) }

newtype MaybeT m a = 
  MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT e m a = 
  ReaderT { runReaderT :: e -> m a }


instance (Functor f) => Functor (IdentityT f) where
  fmap f (IdentityT mma) = IdentityT $ (fmap . fmap) f mma

instance (Functor f) => Functor (MaybeT f) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance (Functor f) => Functor (ReaderT e f) where
  fmap f (ReaderT mma) = ReaderT $ (fmap . fmap) f mma


instance (Applicative f) => Applicative (IdentityT f) where
  pure = IdentityT . pure . pure

  (IdentityT ffab) <*> (IdentityT ffa) = 
    IdentityT $ (<*>) <$> ffab <*> ffa


instance (Applicative f) => Applicative (MaybeT f) where
  pure = MaybeT . pure . pure

  (MaybeT ffab) <*> (MaybeT ffa) = 
    MaybeT $ (<*>) <$> ffab <*> ffa


instance (Applicative f) => Applicative (ReaderT e f) where
  pure = ReaderT . pure . pure

  (ReaderT ffab) <*> (ReaderT ffa) = 
    ReaderT $ (<*>) <$> ffab <*> ffa


instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b

  (IdentityT mma) >>= f = IdentityT $ mma >>= runIdentityT . f . runIdentity


instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a
        -> (a -> MaybeT m b)
        -> MaybeT m b
  (MaybeT mma) >>= f = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)
      
instance (Monad m) => Monad (ReaderT e m) where
  return = pure

  (>>=) :: ReaderT e m a
        -> (a -> ReaderT e m b)
        -> ReaderT e m b
  (ReaderT mma) >>= f = ReaderT $ \r -> do
    a <- mma r
    runReaderT (f a) r


type ID = Int
type DB = M.Map ID String

db :: DB
db = M.fromList [(1, "Alexa"), (2, "Ben"), (3, "Casey")]

dbBad :: DB
dbBad = M.fromList [(1, "Alexa"), (3, "Casey")]

type Program a = ReaderT DB Maybe a -- DB -> Maybe a

safeRead :: ID -> Program String
safeRead = ReaderT . M.lookup

runProgram :: Program a -> DB -> Maybe a
runProgram prog db = (runReaderT prog) db

safeReadRange :: ID -> ID -> Program [String]
safeReadRange idMin idMax = do
  let ids = enumFromTo idMin idMax
  sequenceA $ safeRead <$> ids

validate :: Program ()
validate = do
  n <- ReaderT (Just . M.size)
  names <- safeReadRange 1 n
  return ()


myProgram :: Program String
myProgram = do
  validate
  -- ...
  return "my result"


