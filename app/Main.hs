module Main where

--import State( PuzzleState(Game) )
import CheckComplete(getNextPos, checkPuzzle)
import PuzzleGenerator
import Shuffle

import System.Random
import Data.Time.Clock
import Data.Time.LocalTime
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
rows = 4
cols = 4
gridSize = 32

gridSize', rows', cols' :: Float
gridSize' = 32
rows' = 4
cols' = 4
grid = zip [1..rows] [1..cols]

eachGrid :: Picture -> Float -> Float -> Int -> Color -> Picture
eachGrid pic x y a bgc = scale 0.5 0.5 $ translate (x*gridSize'*3 - 200) (y*gridSize'*3 - 200) $ color bgc $ text (show a) where
    pict = do
        wall <- loadBMP "./src/e0.bmp"
        return wall

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

buffer = [0|x<-[1..cols]]

now = getCurrentTime
timezone = getCurrentTimeZone
g = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    return (mkStdGen minute)

initState :: PuzzleState
initState = Game
    {
    grids = generateRow [] buffer [] 0 1 rows' 1 g ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 1
    }

toFloat :: Int -> Float ->Float
toFloat 0 a = a
toFloat a b = if a>0 then toFloat (a-1) (b+1) else toFloat (a+1) (b-1)

pRenderState :: Picture -> [Int] -> Float -> Float -> Float -> Float -> Float -> Float -> [Picture]
pRenderState pic [] x y r c px py = []
pRenderState pic (a:xs) x y r c px py = (if x == (toFloat (floor px) 0) && y == (toFloat (floor py) 0) then ([eachGrid pic x y a green]) else ([eachGrid pic x y a white])) ++ (pRenderState pic xs (fst (getNextPos x y r c)) (snd (getNextPos x y r c)) r c px py)

renderState :: Picture -> PuzzleState -> Picture
renderState a s = if tempStage == 1 then pictures(pRenderState a (grids s) 1 1 rows' cols' (posx s) (posy s))
    else rectangleSolid gridSize' gridSize' where
        tempStage = (stage s)

updateState :: Float -> PuzzleState -> PuzzleState
updateState _ m = if checkPuzzle (grids m) (crctConfig m) then m{stage = 2}
    else if tempIn == 1 && (posy m) < rows' then m{ posy = (posy m) + 0.1 } 
    else if tempIn == 4 && (posy m) >= 2 then m{ posy = (posy m) - 0.1 } 
    else if tempIn == 2 && (posx m) < cols' then m{ posx = (posx m) + 0.1 } 
    else if tempIn == 8 && (posx m) >= 2 then m{ posx = (posx m) - 0.1 } 
    else m where
        tempIn = (input m)

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
main = do
        wall <- loadBMP "./src/shapes/e0.bmp"
        play window background fps initState (renderState wall) handleInput updateState


