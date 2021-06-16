{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser (parseExp)
import StackVM (Program, StackExp(..), StackVal(..), stackVM)

newtype MinMax  = MinMax Integer deriving (Eq, Ord, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

class Expr a where
    lit :: Integer -> a
    add, mul :: a -> a -> a
    
instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit a
        | a <= 0    = False 
        | a > 0     = True
        | otherwise = False 
    add a b = a || b
    mul a b = a && b

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

instance Expr Mod7 where
    lit a
        | a >= 6    = Mod7 6
        | a <= 0    = Mod7 0
        | otherwise = Mod7 0
    add (Mod7 a) (Mod7 b) = Mod7 (toInteger $ mod (a + b) 7)
    mul (Mod7 a) (Mod7 b) = Mod7 (toInteger $ mod (a * b) 7)    

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7     

eval :: ExprT -> Integer 
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b
eval (ExprT.Lit a)   = a

evalStr :: String -> Maybe Integer
evalStr str =  case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
    Just a  -> Just (eval a)
    Nothing -> Nothing

reify :: ExprT -> ExprT
reify = id

instance Expr Program where
    lit a = [StackVM.PushI a]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul;