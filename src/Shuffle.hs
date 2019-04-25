module Shuffle
    (trans, turn, shuffle
    ) where

import System.Random

trans :: Int -> Int
trans 0 = 0
trans 1 = 2
trans 2 = 4
trans 4 = 8
trans 8 = 1
trans 3 = 6
trans 6 = 12
trans 12 = 9
trans 9 = 3
trans 7 = 14
trans 14 = 13
trans 13 = 11
trans 11 = 7
trans 5 = 10
trans 10 = 5
trans a = a

turn :: Int -> Int -> Int--randomint,piece 
turn 0 b = b
turn 1 b = trans b
turn 2 b = trans (trans b)
turn 3 b = trans (trans (trans b))
turn a b = turn (a`rem` 4) b

shuffle:: [Int] -> StdGen -> [Int]
shuffle [] g = []
shuffle (a:as) g = (turn b a):(shuffle as g2) where
    (b, g2) = randomR (0,3) g