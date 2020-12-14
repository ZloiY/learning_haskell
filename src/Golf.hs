module Golf
    ( skips
    , localMaxima
    , histogram
    ) where

import Data.List

skips :: [a] -> [[a]]
-- Stolen from here https://github.com/bschwb/cis194-solutions/blob/master/03-rec-poly/Golf.hs
skips lst = [each i lst | i <- [1..length lst]]
    where
        each n lst = [lst !! i | i <- [n-1, n-1+n..length lst - 1]]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
    | a < b && b > c = b:localMaxima (b:c:xs)
    | otherwise      = localMaxima (b:c:xs)
localMaxima _ = []

-- createMatrix converts [1,1,1,5] to [[1,5],[1],[1]]
-- result is representation of rows in histogram where the first array is the first row from the bottom,
-- second array is the second row and etc.
histogram :: [Integer] -> String
histogram numbers = convertToString $ reverse $ createMatrix numbers []
    where
        createMatrix numbers subNums 
            | null (numbers \\ subNums)     = [] 
            | null (nub numbers \\ subNums) = createMatrix (numbers \\ subNums) (nub numbers \\ subNums)
            | otherwise                     = (nub numbers \\ subNums) : createMatrix (numbers \\ subNums) (nub numbers \\ subNums)
        createLine nums = [if x `elem` nums then  '*' else ' ' | x <- [0..9]]
        convertToString (nums:numss) = createLine nums ++ '\n':convertToString numss
        convertToString _            = "==========\n0123456789\n"