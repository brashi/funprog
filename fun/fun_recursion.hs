-- Recursion and related stuff
module RecFun where


quickS :: (Ord a) => [a] -> [a]
quickS [] = []
quickS (x:xs) = quickS([a | a <- xs, a <= x]) ++ [x] ++ quickS([a | a <- xs, a > x])

sep1 p xs = (fold p xs, fold (not . p) xs) where fold p xs = foldr (\x -> if p x then (x:) else id) [] xs
sep (p:ps) xs = (fold p xs, fold (not . p) xs) : sep ps xs where fold p xs = foldr (\x -> if (p x) then (x:) else id) [] xs 
sep [] xs = []

-- 6 . Recursion (Hutton)