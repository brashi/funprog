module ExUsing where

import Prelude hiding
    ( filter
    )

type Pred a = (a -> Bool)

test :: (a -> Bool) -> a -> [a]
test p x = [x | p x]
-- using concat
filter :: Pred a -> [a] -> [a]
filter p = concat . map (test p)

-- using zipWith
sorted :: Ord a => [a] -> Bool
sorted = and . (zipWith (<=) <*> tail)

-- using zipWith
fibs :: Integral i => [i]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


