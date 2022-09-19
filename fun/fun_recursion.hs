-- Recursion and related stuff
module RecFun where


quickS :: (Ord a) => [a] -> [a]
quickS [] = []
quickS (x:xs) = quickS([a | a <- xs, a <= x]) ++ [x] ++ quickS([a | a <- xs, a > x])

