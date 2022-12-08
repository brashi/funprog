{-# LANGUAGE InstanceSigs #-}
module Funktor where

import Prelude hiding ( fmap , (<$), (<$>) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const


infixl 4 <$>
(<$>) :: Funktor f => (a -> b) -> f a -> f b
(<$>) = fmap


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

instance Funktor ((->) r) where
    fmap = (.)

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show)

instance Funktor Tree where
    fmap g (Leaf a) = Leaf (g a)
    fmap g (Node x l r) = Node (g x) (fmap g l) (fmap g r)

instance Funktor IO where
    fmap :: (a -> b) -> IO a -> IO b
    fmap f ax =
        do 
            x <- ax
            return (f x)
-- ...define Functor instances of as many * -> * things as you can think of!

