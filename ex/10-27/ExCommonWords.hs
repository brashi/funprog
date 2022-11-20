module ExCommonWords
    ( commonWords
    ) where

-- commonWords 4 "Define the function commonWords,
-- which should receive an Int n and a text and
-- return some string describing the n most common words
-- found in the input text."
--
-- should return something like:
--
-- "the: 3\nand: 2\nn: 2\ntext: 2\n"

-- Use everything you've learnt so far to make your
-- program elegant and nice!

-- do not alter this import
import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Data.Char as C
    -- probably you will not need all of them:
    ( isAlpha , isSymbol , isPunctuation , isSpace , toLower, isLetter, intToDigit )

-- Your imports should go here.
-- Might want to make them qualified;
-- import only what you need from each module!

-- you may only import further entities that YOU have
-- already defined in other modules:

import Sort ( sort )

-- **REPLACE** Data.List by your own home-made ExList!
-- **REPLACE** Data.List by your own home-made ExList!
import ExList2
    -- feel free to remove and/or add entities:
    ( break
    , span
    , concat
    , map
    , filter
    , take
    , dropWhile, length
    , reverse
    )
import Text.Printf (IsChar(toChar))
import Data.Char (intToDigit)

-- Let's start with some type synonyms you might want to use:

type Text = String
type Word = String

-- On with the functions now:

texto = "Define the function commonWords,\nwhich should receive an Int n and a text and\nreturn some string describing the n most common words\nfound in the input text."

commonWords
    :: Int     -- how many common words
    -> Text    -- the input text (book, poem, whatever)
    -> String  -- the output string with the results

commonWords n =
    concatMap showRun . take n .
    sortRuns . countRuns .
    sortWords . words .
    map C.toLower

-- Bonus
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

showRun :: (Int,Word) -> String
showRun (n,w) = concat [w, ":", " ", [intToDigit n], "\n"]

-- if you think this makes your code more readable...
type Run = [(Int,Word)]

count :: (Integral c, Eq a) => a -> [a] -> c
count x = ExList2.length . filter (==x)

countRuns :: [Word] -> [(Int,Word)]
countRuns [] = []
countRuns ws'@(w:ws) = (c,w) : countRuns (filter (/= w) ws)
                    where c = count w ws'

sortWords :: [Word] -> [Word]
sortWords = sort

sortRuns :: [(Int,Word)] -> [(Int,Word)]
sortRuns = reverse . sort     -- is this correct?

words :: Text -> [Word]
words s =
    case dropWhile (not . C.isLetter) s of
         "" -> []
         s' -> let (w, s'') = span C.isLetter s'
                in w : words s''
