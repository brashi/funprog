fibbo :: Integer -> Integer
fibbo 0 = 0
fibbo 1 = 1
fibbo x = fibbo (x-1) + fibbo(x-2)
