module Lib
    ( someFunc
    , toDigitsRev
    , toDigits
    , doubleOther
    , sumDigits
    , validate
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

toDigitsRev :: Integer -> [Integer]
toDigitsRev number
    | number <= 0 = []
    | otherwise   = toArray number
    where
        toArray n
            | (n > 10) && ((n `div` 10) > 0) = (n `mod` 10) : toArray (n `div` 10)
            | n < 10                         = n : []
            | otherwise                      = []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleOther :: [Integer] -> [Integer]
doubleOther arr
    | length arr == 1 = arr
    | length arr > 1 = reverse (doubleOther' (reverse arr))
    where
        doubleOther' (x1 : x2 : xs) = x1 : (x2 * 2) : doubleOther' xs
        doubleOther' (x1 : xs) = x1 : doubleOther' xs
        doubleOther' [] = []

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [a] = a
sumDigits (x : xs)
    | ((x `div` 10) /= 0) && (x /= 10) = (x `mod` 10) + (x `div` 10) + (sumDigits xs) 
    | otherwise = x + (sumDigits xs)

validate :: Integer -> Bool
validate = check . sumDigits . doubleOther . toDigits
    where
        check sum = (sum `mod` 10) == 0