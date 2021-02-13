module H4 ( fun1'
    , fun2'
    , fun1
    , fun2
    , foldTree
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
fun2' n = sum . filter even . takeWhile (> 1) $ iterate isEven n
    where
        isEven n = if even n then n `div` 2 else n * 3 + 1

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
