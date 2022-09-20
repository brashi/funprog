module ExFromWhile where

fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile x p xs = takeWhile p (drop x xs)

fromFor :: Int -> Int -> [a] -> [a]
-- Definição Sem uso do operador de Composição (Fiz por curiosidade :D)
-- fromFor x y xs = take y (drop x xs)
fromFor x y = take y . drop x

fromTo :: Int -> Int -> [a] -> [a]
fromTo x y = take (y-1) . drop x

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
-- Porquê eu não posso simplesmente passar drop x . take y ???
fromToThat x y p xs = filter p ((drop x . take y) xs)
