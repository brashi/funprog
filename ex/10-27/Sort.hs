module Sort
    ( sort
    , msort
    , qsort
    , isort
    , sorted
    ) where

sort :: Ord a => [a] -> [a]
sort = msort

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort zs = merge (msort xs) (msort ys)
    where
        (xs,ys) = halve zs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs'@(x:xs) ys'@(y:ys)
    | x <= y    = x : merge xs ys'
    | otherwise = y : merge xs' ys

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve [x] = ([x],[])
halve (x:y:xs) = (x:lxs,y:rxs)
    where
        (lxs,rxs) = halve xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (w:xs) = qsort low ++ [w] ++ qsort high
    where
        low = filter (<= w) xs
        high = filter (> w) xs

isort :: Ord a => [a] -> [a]
isort = foldr insert []

insert :: Ord a => a -> [a] -> [a]
insert w [] = [w]
insert w xs'@(x:xs)
    | w < x     = w : xs'
    | otherwise = x : insert w xs

sorted :: Ord a => [a] -> Bool
sorted (x:y:ys) = x <= y && sorted (y:ys)
sorted _ = True