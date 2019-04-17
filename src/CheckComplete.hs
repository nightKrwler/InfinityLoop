module CheckComplete
    (getNextPos, checkPuzzle
    ) where


getNextPos :: Float -> Float -> Float -> Float -> (Float, Float)
getNextPos x y r c = 
    if x == c 
        then (1,  (y+1))
    else
        ((x+1), y)

checkPuzzle :: [Int] -> [Int] -> Bool
checkPuzzle [] [] = True
checkPuzzle (a:as) (b:bs) = if a == b then checkPuzzle as bs else False


