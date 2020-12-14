module H4 ( fun1'
    , fun2'
    , fun1
    , fun2
    ) where

fun1 :: [Integer] -> Integer
fun1 []         = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = foldl (\acc val -> (val - 2) * acc) 1 . filter even

fun2' :: Integer -> Integer
fun2' n
    | even n    = sum . takeWhile (/= 1) $ iterate (`div` 2) n
    | otherwise = sum . takeWhile (/= 1) $ iterate (\n -> n * 3 + 1) n