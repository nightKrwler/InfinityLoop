module CheckComplete
    (getNextPos, checkPuzzle
    ) where

import PuzzleGenerator

getNextPos :: Float -> Float -> Float -> Float -> (Float, Float)
getNextPos x y r c = 
    if x == c 
        then (1,  (y+1))
    else
        ((x+1), y)


checkPieces::Int->Int->Int->Bool
checkPieces a da pa = if ((tunnel "u" da) == (tunnel "d" a)) && ((tunnel "r" pa) == (tunnel "l" a)) then True else False

--Whole puzzle, buffer of prev row, buffer of presrow read already, last piece read
-- Checks whether the puzzle is complete
checkPuzzle :: [Int] -> [Int] -> [Int] -> Int -> Float -> Float -> Float -> Bool
{-
checkPuzzle [] [] = True
checkPuzzle a [] = False
checkPuzzle [] a = False
checkPuzzle (a:as) (b:bs) = if a == b then checkPuzzle as bs else False
-}
checkPuzzle [] [] presrow prev row mrow cols = True
checkPuzzle p [] presrow prev row mrow col = if row < mrow then (checkPuzzle p presrow [] 0 (row+1) mrow 1) else if p == [] then True else False
checkPuzzle (p:ps) (b:bs) presrow prev row mrow col  = if lastCheck then (checkPuzzle ps bs (presrow++[p]) p row mrow (col+1) ) else False where
    lastCheck = checkPieces p b prev
checkPuzzle p b presrow prev row mrow col = False
