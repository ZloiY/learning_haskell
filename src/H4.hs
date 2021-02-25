module H4 ( fun1'
    , fun2'
    , fun1
    , fun2
    , foldTree
    , xor
    , map'
    , sieveSundaram
    ) where

import Data.List ( find, (\\) )

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
fun2' n = sum . filter even . takeWhile (> 1) $ iterate isEven n
    where
        isEven n = if even n then n `div` 2 else n * 3 + 1

xor :: [Bool] -> Bool
xor = odd . foldr (\val acc -> if val then acc + 1 else acc) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\val acc -> f val:acc) []

myFold :: (a -> b -> a) -> a -> [b] -> a
myFold f base = foldr (flip f) base . reverse

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) . filterNumbers [1..n] $ notPrimeNumbers n 1 1 

notPrimeNumbers :: Integer -> Integer -> Integer -> [Integer]
notPrimeNumbers n i j
    | i + j + 2 * i * j < n  = i + j + 2 * i * j:notPrimeNumbers n i (j + 1)
    | i + j + 2 * i * j == n = i + j + 2 * i * j:notPrimeNumbers n (i + 1) (i + 1)
    | otherwise              = []

filterNumbers :: [Integer] -> [Integer] -> [Integer]
filterNumbers (x:xs) numbers =
    case find (==x) numbers of
        Just _  -> filterNumbers xs numbers
        Nothing -> x:filterNumbers xs numbers
filterNumbers [] _           = []

-- stole it from here https://github.com/bschwb/cis194-solutions/blob/master/04-higher-order/hw04.hs#L10
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map ((+1) . (*2)) $ [1..n] \\ sieve
  where sieve = map (\(i, j) -> i + j + 2*i*j)
                . filter (\(i, j) -> i + j + 2*i*j <= n)
                $ cartProd [1..n] [1..n]

data Tree a = Leaf 
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

data Side = Left | Right
    deriving (Show)

foldTree :: [a] -> Tree a
foldTree = calcTreeHeight . foldr addNodeToTree Leaf

addNodeToTree :: a -> Tree a -> Tree a
addNodeToTree node tree@(Node _ a n b) = case whichBranchBigger tree of
    H4.Right -> Node 0 (addNodeToTree node a) n b
    H4.Left  -> Node 0 a n (addNodeToTree node b)
addNodeToTree node Leaf = Node 0 Leaf node Leaf

whichBranchBigger :: Tree a -> Side
whichBranchBigger Leaf           = H4.Right
whichBranchBigger (Node _ a _ b) = if treeLength a > treeLength b then H4.Left else H4.Right
    where
        treeLength (Node _ a _ b) = 1 + treeLength a + treeLength b
        treeLength Leaf           = 0
    
calcTreeHeight :: Tree a -> Tree a
calcTreeHeight (Node _ left n right) = Node (branchLength left) (calcTreeHeight left) n (calcTreeHeight right)
calcTreeHeight Leaf = Leaf

branchLength :: Tree a -> Integer
branchLength (Node _ a _ _) = 1 + branchLength a
branchLength Leaf           = 0
