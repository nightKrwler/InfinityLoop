module Shuffle
    (trans
    ) where

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
trans a = a
