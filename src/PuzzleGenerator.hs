module PuzzleGenerator
    (generateRow
    ) where

--        up 1
  --      right 2
    --    down 4
      --  left 8

import CheckComplete(getNextPos)
import System.Random

isOnCorner :: Float -> Float -> (Float, Float) -> Bool
isOnCorner a b (1, 1) = True
isOnCorner a b (x, y) = if a == x && b == y then True else False

isOnEdge :: Float -> Float -> (Float, Float) -> Bool
isOnEdge a b (1, 1) = False
isOnEdge a b (1, y) = True
isOnEdge a b (x, 1) = True
isOnEdge a b (x, y) = if a == x||b == y then True
    else False

tunnel :: [Char] -> Int -> Int
tunnel "u" b = if b `rem` 2 == 0 then 0 else 4
tunnel "r" b = if b `rem` 4 == b `rem` 2 then 0 else 8
tunnel "d" b = if b `rem` 8 == b `rem` 4 then 0 else 1
tunnel "l" b = if b `rem` 16 == b `rem` 8 then 0 else 2
tunnel a b = 0

puzzle = []
prevRow = [1,0]
bufferRow = []

-- Puzzle, bufferow, presrow, lastpiece in row, rowno., maxrowno.,colno.,gen give new row
generateRow  :: [Int] -> [Int] -> [Int] -> Int -> Float -> Float -> Float -> StdGen -> ([Int], StdGen)
generateRow x [] b d row mrow col g' = if row<mrow then (generateRow (x++b) b [] 0 (row+1) mrow 1 g') else (x++b, g')
generateRow x (a:as) b d rows mrow col g' = (generateRow x as (b++[c]) c rows mrow (col+1) g2) where
    (tc, g2) = randomR (0,3) g'
    tb = tunnel "r" d
    ta = tunnel "u" a
    c = if rows == 1 && col == 1 then tc
    else if rows == 1 then tc + tb
    else if col == 1 then tc + ta
    else if rows == mrow && col == 1 then 2*(tc `rem` 2) + ta
    else if rows == mrow && as == []  then ta + tb
    else if rows == mrow then 2*(tc `rem` 2) + tb + ta
    else tb + ta + tc