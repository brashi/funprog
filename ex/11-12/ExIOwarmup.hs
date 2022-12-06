module IOwarmup where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    )
import System.IO hiding
    ( putStrLn
    , putStr
    , getLine
    )

-- what is the type?  (don't cheat)
whatIsMyType =
    do v1 <- getChar
       v2 <- getChar
       putStrLn $ "\nI have all I need:" ++ [v1,v2]
       return [v1,v2]

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do  putChar x
                    putStr xs

putStrLn :: String -> IO ()
putStrLn str = do putStr str
                  putChar '\n'


getLine :: IO String
getLine = do    c <- getChar
                if c == '\n'
                    then return ""
                    else do l <- getLine
                            return (c:l)

putNtimes :: Integral i => i -> Char -> IO ()
putNtimes i c = if i <= 1
                    then putChar c
                    else do putChar c
                            putNtimes (i-1) c

doNtimes :: Integral i => i -> IO a -> IO [a]
doNtimes 0 ac = return []
doNtimes i ac = do
                    ac
                    doNtimes (i-1) ac



doForever :: IO a -> IO ()
doForever act = do
                    act
                    doForever act

when :: Bool -> IO () -> IO ()
when p ac = if p
                then do ac
                else return ()

-- consult read.txt to learn about read
getInteger :: IO Integer
getInteger = do
                inteiro <- getLine
                return (read inteiro :: Integer)

sgetLine :: IO String
sgetLine =  do  x <- getCh
                if x == '\n' then
                    do  putChar x
                        return ""
                else
                    do  putChar '-'
                        xs <- sgetLine
                        return (x:xs)

getCh :: IO Char
getCh = do 
        hSetEcho stdin False
        x <- getChar
        hSetEcho stdin True
        return x