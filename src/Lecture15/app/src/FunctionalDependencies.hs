{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependencies where

class Extract container elem | container -> elem where
  extract :: container -> elem

instance Extract (a, b) a where
  extract (x, _) = x

v = extract ('x', 3 :: Int)
