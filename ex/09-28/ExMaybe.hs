module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M
import System.Win32 (COORD(x))

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:ms) = x : catMaybes ms
catMaybes (Nothing:ms) = catMaybes ms

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Sorry i can't be nothing !"

fromMaybe :: a -> Maybe a -> a
fromMaybe x (Just m) = m
fromMaybe x Nothing = x

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
-- mapMaybe _ [] = []
-- mapMaybe f (x:xs) = if isJust (f x) then fromJust (f x) : mapMaybe f xs else mapMaybe f xs
-- mapMaybe f = map fromJust . filter isJust . map f
mapMaybe f = catMaybes . map f

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x f m | isNothing m = x
            | otherwise = f (fromJust m)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith = undefined

