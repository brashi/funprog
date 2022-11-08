module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

isLeft :: Either a b -> Bool
isLeft (Left a) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right b) = True
isRight _ = False

lefts :: [Either a b] -> [a]
lefts es = map (\(Left x) -> x) $ filter isLeft es

rights :: [Either a b] -> [b]
rights es =  map (\(Right x) -> x) $ filter isRight es

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft d _ = d

fromRight :: b -> Either a b -> b
fromRight = undefined

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = undefined

either :: (a -> c) -> (b -> c) -> Either a b -> c
either = undefined

