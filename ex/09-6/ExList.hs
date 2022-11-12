module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head (x:_) = x
head [] = error "lista vazia"

-- Fazendo o Last
last :: [a] -> a
last [x] = x
last (_:xs) = last xs
last [] = error "lista vazia"

tail :: [a] -> [a]
tail (_:xs) = xs
tail [] = error "lista vazia"

-- Init
init :: [a] -> [a]
init [x] = []
init [] = error "lista vazia"
init (x:xs) = x : init xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs <: x

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
-- Hey, você disse que não teria Lisp no curso !! D=
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc y (x:xs) = x : snoc y xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "Lista vazia."
minimum (x:xs) = foldr min x xs

maximum :: Ord a => [a] -> a
maximum [] = error "Lista vazia."
maximum (x:xs) = foldr max x xs

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take(n -1) xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (x:xs) = drop (n - 1) xs

-- Desta maneira, ou usando um Guards com otherwise = []
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []
takeWhile _ [] = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs
dropWhile _ [] = []


inits :: [a] -> [[a]]
inits (x:xs) = [] : map (x:) (inits xs)
inits [] = [[]]

tails :: [a] -> [[a]]
tails (x:xs) = (x:xs) : tails xs
tails [] = [[]]

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
-- subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)
subsequences (x:xs) =  foldr (\ys r -> ys : (x : ys) : r) [] (subsequences xs)
-- subsequences (x:xs) =  [x] : L.foldr f [] (subsequences xs) where f ys r = ys : (x : ys) : r 

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: (Eq a) => a -> [a] -> Bool
elem y xs = any(== y) xs
-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

(!!) :: [a] -> Int -> a
(!!) [] _ = error "Indice não existe !!"
(!!) (x:xs) 0 = x
(!!) (x:xs) y = (!!) xs (y-1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs


map :: (a -> b) -> [a] -> [b]
map p (x:xs) = p x : map p xs
map _ [] = []

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f [] _ = []
zipWith f _ [] = []

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

lines :: [Char] -> [[Char]]
lines = foldr (\x (y:ys) -> if x == '\n' then "":(y:ys) else (x:y):ys) [[]]

words :: [Char] -> [[Char]]
words = foldr (\x (y:ys) -> if C.isSpace x then "":(y:ys) else (x:y):ys) [[]]

unlines :: [[Char]] -> [Char]
unlines [] = ""
unlines xs = foldr(\x s -> x ++ '\n':s) [] xs

unwords :: [[Char]] -> [Char]
unwords [] = ""
unwords xs = foldr (\x s -> x ++ ' ':s) [] xs

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome [] = True
palindrome xs = palinAux xs == reverse (palinAux xs)

-- Pega apenas as letras e retorna em minúsculo
palinAux :: String -> String
palinAux [] = []
palinAux (x:xs) = if C.isLetter x then C.toLower x : palinAux xs else palinAux xs

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

bonuses palindromes:
"Girafarig"
"Go hang a salami! I'm a lasagna hog!"
"No lemon, no melon."
-}

