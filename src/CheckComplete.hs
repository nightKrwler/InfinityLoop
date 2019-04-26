module CheckComplete
    (getNextPos, checkPuzzle
    ) where


getNextPos :: Float -> Float -> Float -> Float -> (Float, Float)
getNextPos x y r c = 
    if x == c 
        then (1,  (y+1))
    else
        ((x+1), y)


--Whole puzzle, buffer of prev row, buffer of presrow read already, last piece read
checkPuzzle :: [Int] -> [Int] -> Bool
checkPuzzle [] [] a = True
{-
checkPuzzle [] [] = True
checkPuzzle a [] = False
checkPuzzle [] a = False
checkPuzzle (a:as) (b:bs) = if a == b then checkPuzzle as bs else False
-}
checkPuzzle p:ps b:bs presrow  = checkPuzzle



