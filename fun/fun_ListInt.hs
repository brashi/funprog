module ListInt where
import Prelude (Show(..), Int, String)
import Data.List


data ListInt = Empty | Cons Int ListInt

instance Show ListInt where
    show Empty = "" 
    show (Cons x Empty) = show x 
    show (Cons x y) = show x ++ "," ++ show y