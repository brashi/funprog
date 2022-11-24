module ExCurry where

import Prelude hiding ( curry , uncurry )
import System.Win32 (COORD(y))

-- use your mind to infer the types, don't cheat!

-- curry gets a "traditional" binary function
-- and returns its currified version
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x -> \y -> f (x,y)
curry' f x y = f (x,y)

-- uncurry gets a currified function
-- and returns its "traditional" binary version
uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f = \(x,y) -> f x y
uncurry' f (x,y) = f x y

