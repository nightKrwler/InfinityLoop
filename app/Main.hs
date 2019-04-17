module Main where

--import State( PuzzleState(Game) )
import CheckComplete(getNextPos, checkPuzzle)

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 1000
height = 600
offset = -100

window :: Display
window = InWindow "Puzzle" (width, height) (offset, offset)

background:: Color
background = red

gridSize, rows, cols ::Int
rows = 2
cols = 2
gridSize = 32

gridSize', rows', cols' :: Float
gridSize' = 32
rows' = 2
cols' = 2
grid = zip [1..rows] [1..cols]

eachGrid :: Float -> Float -> Int -> Color -> Picture
eachGrid x y a bgc = scale 0.5 0.5 $ translate (x*gridSize'*3 - 200) (y*gridSize'*3 - 200) $ color bgc $ png "e1.png"

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

data PuzzleState = Game
    {
    grids :: [Int],
    crctConfig :: [Int],
    posx, posy :: Float,
    input :: Int,
    stage :: Int
    }

initState :: PuzzleState
initState = Game
    {
    grids = [(x+y+1) `rem` 4| x <- [1..rows], y <- [1..cols]],
    crctConfig = [(x+y) `rem` 4| x <- [1..rows], y <- [1..cols]],
    posx = 1,
    posy = 1,
    input = 0,
    stage = 1
    }

toFloat :: Int -> Float ->Float
toFloat 0 a = a
toFloat a b = if a>0 then toFloat (a-1) (b+1) else toFloat (a+1) (b-1)

pRenderState :: [Int] -> Float -> Float -> Float -> Float -> Float -> Float -> [Picture]
pRenderState [] x y r c px py = []
pRenderState (a:xs) x y r c px py = (if x == (toFloat (floor px) 0) && y == (toFloat (floor py) 0) then ([eachGrid x y a green]) else ([eachGrid x y a white])) ++ (pRenderState xs (fst (getNextPos x y r c)) (snd (getNextPos x y r c)) r c px py)

renderState :: PuzzleState -> Picture
renderState s = pictures(pRenderState (grids s) 1 1 rows' cols' (posx s) (posy s))

updateState :: Float -> PuzzleState -> PuzzleState
updateState _ m = if (input m) == 1 && (posy m) < rows' then m{ posy = (posy m) + 0.1 } 
    else if (input m) == 4 && (posy m) >= 2 then m{ posy = (posy m) - 0.1 } 
    else if (input m) == 2 && (posx m) < cols' then m{ posx = (posx m) + 0.1 } 
    else if (input m) == 8 && (posx m) >= 2 then m{ posx = (posx m) - 0.1 } 
    else m

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

changeN :: Float -> [Int] -> [Int]
changeN _ [] = []
changeN 0 (a:as) = (trans a) : as
changeN i (a:as) = a : changeN (i-1) as


handleInput :: Event -> PuzzleState -> PuzzleState
handleInput (EventKey (SpecialKey KeyUp) (Down) _ _) game = game { input = 1 }
handleInput (EventKey (SpecialKey KeyDown) (Down) _ _) game = game { input = 4 }
handleInput (EventKey (SpecialKey KeyRight) (Down) _ _) game = game { input = 2 }
handleInput (EventKey (SpecialKey KeyLeft) (Down) _ _) game = game { input = 8 }
handleInput (EventKey (SpecialKey KeyUp) (Up) _ _) game = if (input game) == 1 then game { input = 0 } else game
handleInput (EventKey (SpecialKey KeyDown) (Up) _ _) game = if (input game) == 4 then game { input = 0 } else game
handleInput (EventKey (SpecialKey KeyRight) (Up) _ _) game = if (input game) == 2 then game { input = 0 } else game
handleInput (EventKey (SpecialKey KeyLeft) (Up) _ _) game = if (input game) == 8 then game { input = 0 } else game
handleInput (EventKey (SpecialKey KeyEnter) (Down) _ _) game = if (posx game) > 0 then game { grids = (changeN (((toFloat (floor (posy game)) 0) - 1)*rows'+ (toFloat (floor (posx game)) 0)-1 ) (grids game)) } else game
handleInput _ game = game 

fps :: Int
fps = 60

main :: IO ()
main =
    play window background fps initState renderState handleInput updateState


