module Cards
( CardNum(..)
, CardSuit(..)
, Card(..)
, isMinor
, sameSuit
, beats
, beats2
, beatsList
, points
) where

import Data.List

data CardNum
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Read, Show, Eq, Enum, Ord)
 
data CardSuit
    = Diamonds -- бубны
    | Hearts   -- червы
    | Spades   -- пики
    | Clubs    -- трефы
    deriving (Read, Show, Eq)
 
data Card = Card CardNum CardSuit deriving (Read, Show, Eq)
 
 
isMinor :: Card -> Bool
isMinor (Card Two _)  =  True
isMinor  _            =  False 

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2)  =  s1 == s2

beats :: Card -> Card -> Bool
beats (Card n1 s1) (Card n2 s2)  =  s1 == s2 && n1 > n2

beats2 :: Card -> Card -> CardSuit -> Bool
beats2 (Card n1 s1) (Card n2 s2) s
    | s1 == s2   =  n1 > n2
    | s1 == s    =  True
    | otherwise  =  False

beatsList :: [Card] -> Card -> CardSuit -> [Card]
beatsList cs c s  =  filter (\x -> beats2 x c s) cs



crossMap :: (a -> b -> c) -> [a] -> [b] -> [c]
crossMap fn xs ys  =  rec [] xs ys
    where
        rec rs   _      []    =  rs
        rec rs   []   (y:ys)  =  rec rs xs ys
        rec rs (x:xs)   ys    =  rec ((x `fn` head ys) : rs) xs ys

addPoints :: [Int] -> [Int] -> [Int]
addPoints  =  crossMap (+)
 
pointsOf :: CardNum -> [Int]
pointsOf Jack   =  [10]
pointsOf Queen  =  [10]
pointsOf King   =  [10]
pointsOf Ace    =  [1, 11]
pointsOf cn     =  [fromEnum cn + 2]

points :: [CardNum] -> [Int]
points xs  =  sort $ nub $ rec [] xs
    where
        rec rs   []    =  rs
        rec [] (x:xs)  =  rec (pointsOf x) xs
        rec rs (x:xs)  =  rec (addPoints rs $ pointsOf x) xs