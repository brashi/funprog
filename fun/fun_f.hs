-- Exercícios Hutton cap. 4

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = length xs `div` 2

third :: [a] -> a
third xs = head (tail (tail xs))
third' xs = xs !! 2
third'' (_:_:xs:x) = xs


safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs
safetail' xs | null xs = []
            | otherwise = tail xs
safetail'' [] = []
safetail'' xs = tail xs


(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

True || b = True
False || b = b

quest x y = if x && y then True else if False && True then True else if False && False then True else False

questA x = if x && True then True else if False && x then True else False
