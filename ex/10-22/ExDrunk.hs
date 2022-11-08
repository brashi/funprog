module ExDrunk
    ( atIndices
    , everyOther
    , disjoint
    , stretch
    , drunk
    ) where
import System.Win32 (COORD(x))
import Prelude hiding (drop)
import Data.List (intersect)

-- example:
-- atIndices [1,4,5] "Tchauzinho"
-- = "cuz"
atIndices :: Integral i => [i] -> [a] -> [a]
atIndices (i:is) xs = find i xs : atIndices is xs
atIndices [] _ = []

find :: (Eq a, Num a) => a -> [b] -> b
find 0 (x:xs) = x
find _ [] = error "indice fora"
find i (x:xs) = find (i-1) xs

-- example:
-- everyOther 2 "Hello There"
-- = "HloTee"
everyOther :: Integral i => i -> [a] -> [a]
everyOther _ [] = []
everyOther i (x:xs) = x : everyOther i (drop (i-1) xs)

drop :: (Eq i, Num i) => i -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (x:xs) = drop (n - 1) xs

-- examples:
-- disjoint [1,5,9] [2 .. 6]
-- = False
-- disjoint [1,5,9] [2,4 ..]
-- = True
-- ASSUMPTIONS FOR disjoint xs ys:
--   xs and ys are sorted
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint xs ys = null $ intersect xs ys

-- example:
-- stretch 3 "Gustavo"
-- = "GGGuuussstttaaavvvooo"
stretch :: Integral i => i -> [a] -> [a]
stretch i = foldr (\z r -> (replicateS i z) ++ r) []

replicateS :: (Eq i, Num i) => i -> a -> [a]
replicateS 0 x = []
replicateS i x = x : replicateS (i - 1) x

-- example:
-- drunk 3 "Gustavo"
-- = "GusGtuasvtoavo"
-- drunk 5 "Gustavo"
-- = "GustaGvuostavo"
-- To understand these string, either get drunk or look at the markings:
--       , , , , ,,,
--   "GusGtuasvtoavo"
--    ''' ' ' ' '
--         , , ,,,,,
--   "GustaGvuostavo"
--    ''''' ' '
drunk :: Integral i => i -> [a] -> [a]
drunk = undefined

