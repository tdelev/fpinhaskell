module Main where

import Lib
import Text.Printf

main :: IO ()
main = putStr "Hello Haskell\n"

abs :: Int -> Int
abs x 
    | x < 0 = 0 - x
    | otherwise = x


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

format :: String -> Int -> (Int -> Int) -> String
format msg x f = printf msg x (f x)

formatAbs :: Int -> String
formatAbs x = format "%d absolute is %d" x Main.abs

find :: [a] -> (a -> Bool) -> Int
find [] _ = 0
find (head:tail) predicate
    | predicate head = 1 + find tail predicate
    | otherwise = find tail predicate
    -- then 1 + find xs pred
    -- else find xs pred


isSorted :: [a] -> (a -> a -> Bool) -> Bool
isSorted [] _ = True
isSorted [head] _ = True
isSorted (first:(second:tail)) predicate
    | predicate first second = isSorted (second:tail) predicate
    | otherwise = False