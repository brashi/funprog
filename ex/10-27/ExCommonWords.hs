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
    ( isAlpha , isSymbol , isPunctuation , isSpace , toLower )

-- Your imports should go here.
-- Might want to make them qualified;
-- import only what you need from each module!

-- you may only import further entities that YOU have
-- already defined in other modules:

import Sort ( sort )

-- **REPLACE** Data.List by your own home-made ExList!
import ExList2
    -- feel free to remove and/or add entities:
    ( break
    , span
    , concat
    , map
    , filter
    , take
    , dropWhile
    )

-- Let's start with some type synonyms you might want to use:

type Text = String
type Word = String

-- On with the functions now:


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
showRun (n,w) = w

-- if you think this makes your code more readable...
type Run = [(Int,Word)]

countRuns :: [Word] -> [(Int,Word)]
countRuns ws = undefined

sortWords :: [Word] -> [Word]
sortWords = sort    -- is this correct? (I guess so ?)

sortRuns :: [(Int,Word)] -> [(Int,Word)]
sortRuns = sort     -- is this correct?

words :: Text -> [Word]
words s = case dropWhile C.isSpace s of
                "" -> []
                s' -> w : words s''
                    where (w, s'') =
                            break C.isSpace s'
