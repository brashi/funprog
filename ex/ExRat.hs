module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

-- define Rat:
data Rat = Rat {a::Integer, b::Integer } 

racional1 = Rat 1 2
racional2 = Rat 2 4
racional3 = Rat 100 200
racional4 = Rat 1 3

instance Show Rat where
    show rat = show(a rat) ++ "/" ++ show(b rat)

instance Eq Rat where
    (==) x y = (a x `div` b x) == (a y `div` b y)

instance Num Rat where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Ord Rat where
    compare = undefined

rat :: Integer -> Integer -> Rat
rat = undefined

(//) :: Rat -> Rat -> Rat
(//) = undefined

denominator :: Rat -> Integer
denominator = undefined

numerator :: Rat -> Integer
numerator = undefined

