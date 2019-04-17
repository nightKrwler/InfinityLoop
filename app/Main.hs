module Main where

--import State( PuzzleState(Game) )
import CheckComplete(getNextPos, checkPuzzle)

import Graphics.Gloss
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
rows = 10
cols = 10
gridSize = 32

gridSize', rows', cols' :: Float
gridSize' = 32
rows' = 10
cols' = 10
grid = zip [1..rows] [1..cols]

eachGrid :: Float -> Float -> Color -> Picture
eachGrid x y bgc = translate (x*gridSize' - 200) (y*gridSize' - 200) $ color bgc $ rectangleSolid gridSize' gridSize'

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

data PuzzleState = Game
    {
    grids :: [Int],
    configs :: [Int],
    posx, posy :: Float,
    input :: Int
    }

initState :: PuzzleState
initState = Game
    {
    grids = [(x+y) `rem` 4| x <- [1..rows], y <- [1..cols]],
    configs = [0| x <- [1..rows], y <- [1..cols]],
    posx = 1,
    posy = 1,
    input = 0
    }

toFloat :: Int -> Float ->Float
toFloat 0 a = a
toFloat a b = toFloat (a-1) (b+1)

pRenderState :: [Int] -> Float -> Float -> Float -> Float -> Float -> Float -> [Picture]
pRenderState [] x y r c px py = []
pRenderState (0:xs) x y r c px py = (if x == (toFloat (floor px) 0) && y == (toFloat (floor py) 0) then ([eachGrid x y green]) else ([eachGrid x y white])) ++ (pRenderState xs (fst (getNextPos x y r c)) (snd (getNextPos x y r c)) r c px py)
pRenderState (1:xs) x y r c px py = (if x == (toFloat (floor px) 0) && y == (toFloat (floor py) 0) then ([eachGrid x y green]) else ([eachGrid x y yellow])) ++ (pRenderState xs (fst (getNextPos x y r c)) (snd (getNextPos x y r c)) r c px py)
pRenderState (2:xs) x y r c px py = (if x == (toFloat (floor px) 0) && y == (toFloat (floor py) 0) then ([eachGrid x y green]) else ([eachGrid x y black])) ++ (pRenderState xs (fst (getNextPos x y r c)) (snd (getNextPos x y r c)) r c px py)
pRenderState (3:xs) x y r c px py = (if x == (toFloat (floor px) 0) && y == (toFloat (floor py) 0) then ([eachGrid x y green]) else ([eachGrid x y blue])) ++ (pRenderState xs (fst (getNextPos x y r c)) (snd (getNextPos x y r c)) r c px py)
pRenderState (notx:xs) x y r c px py = []

renderState :: PuzzleState -> Picture
renderState s = pictures(pRenderState (grids s) 1 1 rows' cols' (posx s) (posy s))

updateState :: Float -> PuzzleState -> PuzzleState
updateState _ m = if (input m) == 1 then m{ posy = (posy m) + 0.1 } else if (input m) == 4 then m{ posy = (posy m) - 0.1 } else if (input m) == 2 then m{ posx = (posx m) + 0.1 } else if (input m) == 8 then m{ posx = (posx m) - 0.1 } else m

changeN :: Float -> [Int] -> [Int]
changeN _ [] = []
changeN 0 (a:as) = ((a+1) `rem` 4) : as
changeN i (a:as) = a : changeN (i-1) as


handleInput :: Event -> PuzzleState -> PuzzleState
handleInput (EventKey (SpecialKey KeyUp) (Down) _ _) game = if (posy game) < rows' then game { input = 1 } else game{ input = 0 }
handleInput (EventKey (SpecialKey KeyDown) (Down) _ _) game = if (posy game) > 1 then game { input = 4 } else game{ input = 0 }
handleInput (EventKey (SpecialKey KeyRight) (Down) _ _) game = if (posx game) < cols'-1 then game { input = 2 } else game{ input = 0 }
handleInput (EventKey (SpecialKey KeyLeft) (Down) _ _) game = if (posx game) > 1 then game { input = 8 } else game{ input = 0 }
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


