{-# LANGUAGE InstanceSigs #-}
module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

-- define Rat:
data Rat = Rat {a::Integer, b::Integer }

rat1 = rat 1 2
rat2 = rat 2 4
rat3 = rat 100 200
rat4 = rat 1 3

reduce :: Rat -> (Integer,Integer)
reduce r1 = reduce' (a r1) (b r1)

reduce' :: Integral a => a -> a -> (a, a)
reduce' a b = (a `quot` d, b `quot` d) where d = a `gcd` b

instance Show Rat where
    show rat = show(a rat) ++ "/" ++ show(b rat)

instance Eq Rat where
    (==) x y = reduce x == reduce y

instance Num Rat where
    (+) r1 r2 = rat' (reduce' (x * y' + x' * y) (y*y'))
        where (x,x',y,y') = (numerator r1, numerator r2, denominator r1, denominator r2)
    (*) r1 r2 = rat' (reduce' (x * x') (y * y'))
        where (x,x',y,y') = (numerator r1, numerator r2, denominator r1, denominator r2) 
    negate :: Rat -> Rat
    negate r = rat (-a r) (b r)
    abs r = rat' (reduce' (abs a) (abs b)) where (a,b) = (numerator r, denominator r)
    signum r = rat (signum (a r)) 1
    fromInteger x = rat x 1

instance Ord Rat where
    compare = undefined

rat :: Integer -> Integer -> Rat
rat a 0 = error "Denominador Zero é proíbido por Lei segundo a cabeça limitada deste aluno =("
rat a b = Rat a b
rat' :: (Integer, Integer) -> Rat
rat' (a,b) = Rat a b

(//) :: Rat -> Rat -> Rat
(//) r1 r2 = rat' (reduce' (a * d) (b * c)) where (a,b,c,d) = (numerator r1, denominator r1, numerator r2, denominator r2)

denominator :: Rat -> Integer
denominator = b

numerator :: Rat -> Integer
numerator = a


