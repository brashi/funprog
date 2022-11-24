{-# LANGUAGE InstanceSigs #-}
module Funktor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const


instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap g (Just a) = Just (g a)

instance Funktor (Either a) where
    fmap g (Left e) = Left e
    fmap g (Right x) = Right (g x)


data Pair a = Pair a a

instance Funktor ((,) e) where
      fmap :: (a -> b) -> (e, a) -> (e, b)
      fmap f (x,y) = (x, f y)

-- what about Trees?

-- ...define Functor instances of as many * -> * things as you can think of!

