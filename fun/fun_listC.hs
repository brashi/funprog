-- Sobre List Comprehension

pares = [x | x <- [1..80], x `mod` 2 == 0]
impares = [x | x <- [1..80], odd x]


