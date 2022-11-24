{-# LANGUAGE GADTs                         #-}
{-# LANGUAGE DeriveFunctor                 #-}

module Foldinhos where

-- Define folds and unfolds for all the following types.
-- Use folds and unfolds to define new (and old!) functions
-- for each type.
-- Be creative; have fun.

import Prelude hiding
    ( foldr1
-- you will probably hide more entities here:
--  , etc.
--  , etc.
    )


----------------------------------------------------------------
-- List
----------------------------------------------------------------

data L a where
    E :: L a
    C :: a -> L a -> L a
  deriving ( Show , Eq , Functor )

foldL :: b              -- empty
      -> (a -> b -> b)  -- cons
      -> L a -> b
foldL e c E        = e
foldL e c (C x xs) = c x (foldL e c xs)

unfoldL :: (b -> Bool) -- decider (of constructor)
        -> (b -> a)    -- headizer
        -> (b -> b)    -- tailizer
        -> b -> L a
unfoldL d h t x =
    if d x
       then E
       else (h x) `C` (unfoldL d h t (t x))


----------------------------------------------------------------
-- Lazy nats
----------------------------------------------------------------

data N where
    Z :: N
    S :: N -> N
  deriving ( Show , Eq )

foldN :: b          -- zero
      -> (b -> b)   -- succ
      -> N -> b
foldN z s Z     = z
foldN z s (S n) = s $ foldN z s n


----------------------------------------------------------------
-- Binary trees with same type of value on nodes and leaves
----------------------------------------------------------------

data T a where
    Leaf :: a -> T a
    Node :: a -> T a -> T a -> T a
  deriving ( Show , Eq , Functor )

foldT :: (a -> b)            -- leaf
      -> (a -> b -> b -> b)  -- node
      -> T a -> b
foldT lf nd (Leaf v) = lf v
foldT lf nd (Node v tl tr) =
    nd v (foldT lf nd tl) (foldT lf nd tr)

-- some example trees to play with
tree1, tree2 :: T Int

tree1 = Node 1
         (Node 2
            (Leaf 3)
            (Leaf 4))
         (Node 5
            (Leaf 6)
            (Leaf 2))

tree2 = Node 121
          (Node 31
             (Node 5
                (Leaf 1)
                (Leaf 7))
             (Node 42
                (Leaf 32)
                (Leaf 96)))
          (Node 128
             (Leaf 126)
             (Leaf 256))


----------------------------------------------------------------
-- maybe
----------------------------------------------------------------

data M a where
    No :: M a
    Ju :: a -> M a
  deriving ( Show , Eq , Functor )


----------------------------------------------------------------
-- either
----------------------------------------------------------------

data E a b where
    L :: a -> E a b
    R :: b -> E a b
  deriving ( Show , Eq , Functor )

foldE :: (a -> c) -- left
      -> (b -> c) -- right
      -> E a b
      -> c
foldE l r (L x) = l x
foldE l r (R x) = r x


----------------------------------------------------------------
-- booleans
----------------------------------------------------------------

data B where
    F :: B
    T :: B
  deriving ( Show , Eq , Ord )


----------------------------------------------------------------
-- unit
----------------------------------------------------------------

data U where
    U :: U
  deriving ( Show , Eq , Ord )


----------------------------------------------------------------
-- non-empty lists
----------------------------------------------------------------

data NE a where
    Sg :: a -> NE a             -- singleton list
    Cs :: a -> NE a -> NE a     -- cons
  deriving ( Show , Eq , Functor )

foldNE :: (a -> b) -> (a -> b -> b) -> NE a -> b
foldNE sg cs (Sg x)    = sg x
foldNE sg cs (Cs x xs) = cs x (foldNE sg cs xs)

foldr1 :: (a -> a -> a) -> NE a -> a
foldr1 = foldNE id


----------------------------------------------------------------
-- infinite lists
----------------------------------------------------------------

infixr 5 :#
data IL a where
    (:#) :: a -> IL a -> IL a
  deriving ( Show , Eq , Functor )

-- destructors
ihead :: IL a -> a
ihead (x :# _) = x

itail :: IL a -> IL a
itail (_ :# xs) = xs

-- tools
always :: a -> IL a
always x = x :# always x

toList :: IL a -> L a
toList (x :# xs) = x `C` toList xs

-- examples (can you define them as unfolds?)
zeros :: IL N
zeros = always Z

nats :: IL N
nats = Z :# fmap S nats


----------------------------------------------------------------
-- pairs
----------------------------------------------------------------

data P a b where
    P :: a -> b -> P a b
  deriving ( Show , Eq , Functor )

-- destructors
pfst :: P a b -> a
pfst (P x y) = x

psnd :: P a b -> b
psnd (P x y) = y


----------------------------------------------------------------
-- weird type
----------------------------------------------------------------

data W a b where
    W0 :: W a b -> W a b -> W a b
    W1 :: a -> W a b -> W a b
    W2 :: a -> b -> W a b
  deriving ( Show , Eq , Functor )

