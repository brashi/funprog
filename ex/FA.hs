{-# LANGUAGE InstanceSigs #-}
module FA where

import Prelude hiding
    ( fmap
    , (<$)
    , (<$>)
    , Functor(..)
    , Applicative(..)
    , (<*>)
    , (<*)
    , (*>)
    , liftA , liftA2 , liftA3
    )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const

-- LAWS
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Funktor ((,) e) where
    fmap f (x,y) = (x, f y)

instance Funktor ((->) r) where
    fmap = (.)

instance Funktor (Either a) where
    fmap :: (a2 -> b) -> Either a1 a2 -> Either a1 b
    fmap f (Left e) = Left e
    fmap f (Right x) = Right (f x)

instance Funktor IO where
    fmap f ax =
        do  x <- ax
            return (f x)

class Funktor f => Applikative f where
  pure  :: a -> f a

  infixl 4 <*>

  (<*>) :: f (a -> b) -> f a -> f b

  -- LAWS
  -- pure id <*> v = v
  -- pure f <*> pure x = pure (f x)
  -- u <*> pure y = pure ($ y) <*> u
  -- u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

instance Applikative Maybe where
    pure = Just

    Just f <*> Just x = Just $ f x
    _ <*> _ = Nothing

instance Applikative [] where
    pure x = [x]
    fs <*> xs = [ f x | f <- fs , x <- xs ]

newtype ZipList a = ZipList [a]

instance Funktor ZipList where
    fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applikative ZipList where
    pure x = ZipList [x]
    (ZipList fs) <*> (ZipList xs) = ZipList ( zipWith ($) fs xs)

instance Applikative IO where
    pure = return
    mg <*> mx = do  g <- mg
                    x <- mx
                    return $ g x

instance Monoid m => Applikative ((,) m) where
    pure = undefined
    (<*>) = undefined

instance Applikative ((->) r) where
    pure = const
    (<*>) f g x = f x (g x)

instance Semigroup e => Applikative (Either e) where
    pure = undefined
    (<*>) = undefined

-- (In how many sensible ways) can you define Applicative instances
-- for your Tree datatypes?

-- Define the following:
-- You should study Typeclassopedia:
-- https://wiki.haskell.org/Typeclassopedia#Applicative

liftA :: Applikative f => (a -> b) -> f a -> f b
liftA = undefined

liftA2 :: Applikative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 = undefined

(*>) :: Applikative f => f a -> f b -> f b
(*>) = undefined

(<*) :: Applikative f => f a -> f b -> f a
(<*) = undefined

(<**>) :: Applikative f => f a -> f (a -> b) -> f b
(<**>) = undefined

when :: Applikative f => Bool -> f () -> f ()
when = undefined

unless :: Applikative f => Bool -> f () -> f ()
unless = undefined

sequenceAL :: Applikative f => [f a] -> f [a]
sequenceAL = undefined

