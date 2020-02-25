{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependencies where

class Extract container elem where -- | container -> elem where
  extract :: container -> elem

instance Extract (a, b) a where
  extract (x, _) = x

instance Extract (a, b) b where
  extract = undefined

-- v = extract ('x', 3 :: Int)
