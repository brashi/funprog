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
safetail'' (x:xs) = xs


(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

questA x y = if x && y then True else if False && True then True else if False && False then True else False

questB x = if x && True then True else if False && x then True else False

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

mult' = \x -> (\y -> (\z -> x*y*z))

luhnDouble :: Int -> Int
luhnDouble x = if (2*x) > 9 then 2*x - 9 else 2*x
luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z u = sum [luhnDouble x, y, luhnDouble z, u] `mod` 10 == 0

teste x y z u = sum [luhnDouble x, y, luhnDouble z, u]