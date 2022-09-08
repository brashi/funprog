module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )
import Distribution.Compat.Lens (_1)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero = "0"
    show (Succ x) = "S" ++ show x

instance Eq Nat where
    (==) Zero Zero = True
    (==) (Succ x) (Succ y) = x == y
    (==) _ _ = False

x = Succ (Succ Zero)
y = Succ (Succ (Succ (Succ (Succ Zero))))

instance Ord Nat where

    (<=) x y = min x y == x

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min Zero _ = Zero
    min _ Zero = Zero
    min (Succ m) (Succ n) = Succ (min m n)

    max Zero n = n
    max n Zero = n
    max (Succ m) (Succ n) = Succ (max m n)

isZero :: Nat -> Bool
isZero x = x == Zero

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

even :: Nat -> Bool
even Zero = True
even n = odd (pred n)

odd :: Nat -> Bool
odd Zero = False
odd n = even (pred n)

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero x = x
(<+>) x Zero = x
(<+>) x (Succ y) = Succ (x <+> y)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) x (Succ y) = pred (x <-> y)
(<->) Zero x = Zero
(<->) x Zero = x

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) x (Succ y) = (x <*> y) <+> x
(<*>) x Zero = Zero

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) x Zero = Succ Zero
(<^>) x (Succ y) = (x <^> y) <*> x

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) x (Succ y) =  (x <-> Succ y) <*> y 
(</>) x Zero = x

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = undefined
        | x == 0    = undefined
        | otherwise = undefined

