module ListInt where
import Prelude (Show(..), Int, String, Bool, undefined)
import Data.List
import GHC.Num
import Data.Ord


data ListInt = Empty | Cons Int (ListInt)

lista1 = Cons 1 (Cons 2 (Cons 3 Empty))
instance Show ListInt where
    show Empty = "Empty"
    show (Cons x Empty) = "Cons " ++ show x
    show (Cons x y) = "Cons " ++ show x ++ " (" ++ show y ++ ")"

-- Brincando um pouco mais com definições de Array e Lista.
toArray :: ListInt  -> [Int]
toArray Empty = []
toArray (Cons x Empty) = x : []
toArray (Cons x y) = x : toArray y

len :: ListInt  -> Int
len Empty = 0
len (Cons x y) = 1 + len y

dropL :: Int -> ListInt  -> ListInt 
dropL 0 xs = xs
dropL n Empty = Empty
dropL n (Cons x y) = dropL (n - 1) y

mapL :: (Int -> Int) -> ListInt -> ListInt
mapL _ Empty = Empty
mapL p (Cons x y) = Cons (p x) (mapL p y)

filterL :: (Int -> Bool) -> ListInt -> ListInt
filterL _ Empty = Empty
filterL p (Cons x y) = if p x then Cons x (filterL p y) else filterL p y

headL :: ListInt -> Int
headL Empty = undefined -- Uma pequena trapaça...
headL (Cons x y) = x

(+++) :: ListInt -> ListInt -> ListInt

(+++) (Cons x y) (Cons x' Empty) = Cons x' (Cons x y)
(+++) (Cons x y) (Cons x' y') = Cons x y +++ y'