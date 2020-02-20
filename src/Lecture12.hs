{-# Language InstanceSigs #-}

module Lecture12 where

import Data.Functor.Identity
import Control.Monad

-- data Identity a = Identity a
-- data Maybe a = Just a | Nothing

newtype IdentityT m a =
  IdentityT { runIdentityT :: m (Identity a) }

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

-- Functor

instance (Functor f) => Functor (IdentityT f) where
  fmap f (IdentityT mma) = IdentityT $ (fmap . fmap) f mma

instance (Functor f) => Functor (MaybeT f) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

-- Applicative

instance (Applicative f) => Applicative (IdentityT f) where
  pure = IdentityT . pure . pure

  (IdentityT ffab) <*> (IdentityT ffa) =
    IdentityT $ (<*>) <$> ffab <*> ffa

instance (Applicative f) => Applicative (MaybeT f) where
  pure = MaybeT . pure . pure

  (MaybeT ffab) <*> (MaybeT ffa) =
    MaybeT $ (<*>) <$> ffab <*> ffa

-- Monad

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a ->        -- m (Identity a)
           (a -> IdentityT m b) -> -- a -> m (Identity b)
           IdentityT m b           -- m (Identity b)
  (IdentityT mma) >>= f = undefined -- IdentityT $ mma >>= runIdentityT . f
--    IdentityT $ mma >>= runIdentityT . f

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a  -> (a -> MaybeT m b)  -> MaybeT m b
  --  ~ :: m (Maybe a) -> (a -> m (Maybe a)) -> m (Maybe a)
  (MaybeT mma) >>= f = 
    MaybeT $ do
      ma <- mma
      case ma of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)
