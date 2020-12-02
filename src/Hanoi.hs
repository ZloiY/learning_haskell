module Hanoi 
    ( hanoi
    , hanoiQuad
    ) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

hanoiQuad :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiQuad 0 _ _ _ _ = []
hanoiQuad 1 a b _ _ = [(a,b)]
hanoiQuad n a b c d = hanoiQuad (n-2) a c d b ++ [(a,d), (a,b), (d,b)] ++ hanoiQuad (n-2) c b a d