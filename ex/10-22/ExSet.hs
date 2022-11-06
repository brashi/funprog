module ExSet
    ( Set
    , empty
    , singleton
    , fromList
    , toList
    , powerSet
    , insert
    , delete
    , member
    , notMember
    , Prelude.null
    , size
    , isSubsetOf
    , isProperSubsetOf
    , disjoint
    , pairwiseDisjoint
    , union
    , inter
    , (\\)
    , unions
    , inters
    , cartesianProduct
    , disjointUnion
    , Prelude.filter
    , partition
    , Prelude.map
    ) where

import qualified Data.List as L

data Set a = Set [a]

-- CAUTION: you may need to add constraints to your types and instances!

instance Eq (Set a) where
    xs == ys  = undefined

instance Show a => Show (Set a) where
    show xs = show (toList xs)

-- smart constructor
set :: Eq a => [a] -> Set a
set = fromList

empty :: Set a -> Bool
empty (Set []) = True
empty _ = False

singleton :: a -> Set a
singleton x = Set [x]

fromList :: Eq a => [a] -> Set a
fromList xs = Set (stripSet xs)

stripSet :: Eq a => [a] -> [a]
stripSet [] = []
stripSet (x:xs) | x `elem` xs = stripSet xs
                | otherwise = x : stripSet xs

toList :: Set a -> [a]
toList (Set a) = a

powerSet :: Set a -> Set (Set a)
powerSet = undefined

insert :: a -> Set a -> Set a
insert = undefined

delete :: a -> Set a -> Set a
delete = undefined

member :: Eq a => a -> Set a -> Bool
member _ (Set []) = False
member a (Set (x:xs)) = (x == a) || member a (Set xs)

notMember :: Set a -> Bool
notMember = undefined

null :: Set a -> Bool
null = undefined

size :: Integral i => Set a -> i
size = undefined

isSubsetOf :: Set a -> Set a -> Bool
isSubsetOf = undefined

isProperSubsetOf :: Set a -> Set a -> Bool
isProperSubsetOf = undefined

disjoint :: Set a -> Set a -> Bool
disjoint = undefined

pairwiseDisjoint :: Set (Set a) -> Bool
pairwiseDisjoint = undefined

union :: Set a -> Set a -> Set a
union = undefined

inter :: Set a -> Set a -> Set a
inter = undefined

-- relative complement (set difference)
setminus :: Set a -> Set a -> Set a
setminus = undefined

(\\) = setminus
infixr 5 \\

unions :: Set (Set a) -> Set a
unions = undefined

inters :: Set (Set a) -> Set a
inters = undefined

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct = undefined

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion = undefined

filter :: (a -> Bool) -> Set a -> Set a
filter = undefined

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition = undefined

map :: (a -> b) -> Set a -> Set b
map = undefined

