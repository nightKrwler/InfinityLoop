module Main where

--import State( PuzzleState(Game) )
import CheckComplete(getNextPos, checkPuzzle)
import PuzzleGenerator
import Shuffle
import Levels

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
window = FullScreen

background:: Color
background = white

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
eachGrid pic x y a bgc = scale 0.5 0.5 $ translate (x*gridSize'*4.5 - 200) (y*gridSize'*4.5 - 200) $ color bgc $ text (show a) where
    pict = do
        wall <- loadBMP "./src/e0.bmp"
        return wall

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs


buffer = [0|x<-[1..cols]]

now = getCurrentTime
timezone = getCurrentTimeZone
g = mkStdGen 9
    {-do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    return (mkStdGen minute)-}

initState :: PuzzleState
initState = Game
    {
    grids = [],
    crctConfig = [1],
    posx = 1.5,
    posy = 1.5,
    input = 0,
    stage = 0,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }

toFloat :: Int -> Float ->Float
toFloat 0 a = a
toFloat a b = if a>0 then toFloat (a-1) (b+1) else toFloat (a+1) (b-1)

pRenderState :: Picture -> [Int] -> Float -> Float -> Float -> Float -> Float -> Float -> [Picture]
pRenderState pic [] x y r c px py = []
pRenderState pic (a:xs) x y r c px py = (if x == (toFloat (floor px) 0) && y == (toFloat (floor py) 0) then ([eachGrid pic x y a green]) else ([eachGrid pic x y a black])) ++ (pRenderState pic xs (fst (getNextPos x y r c)) (snd (getNextPos x y r c)) r c px py)

renderState :: Picture -> PuzzleState -> Picture
renderState a s =
    if tempStage == 0 then color blue $ rectangleSolid 100 100 
    else if tempStage == 1 || tempStage == 2 || tempStage == 3 then  rectangleSolid 100 100
    else if tempStage == 4 then color yellow $ rectangleSolid 100 100
    else pictures((pRenderState a (grids s) 1 1 rows' cols' (posx s) (posy s))++ [scale 0.5 0.5 $ text (show (moves s) ++ show (best s))]) where
        tempStage = (stage s)

generatePuzzle :: [Int] -> [Int] -> [Int] -> Int -> Float -> Float -> Float -> StdGen -> ([Int], [Int], StdGen)
generatePuzzle x y b d row mrow col g' = (puz, shufpuz, newgen) where
    (puz, newgen) = generateRow x y b d row mrow col g'
    shufpuz = shuffle puz newgen

updateState :: Float -> PuzzleState -> PuzzleState
updateState s m =
    if st == 5 then m{crctConfig = temp2, grids = temp1, randGen = temp3, stage = 6}
    else if checkPuzzle (grids m) (crctConfig m) then m{stage = 4}
    else if tempIn == 1 && (posy m) < rows' then m{ posy = (posy m) + s } 
    else if tempIn == 4 && (posy m) >= 2 then m{ posy = (posy m) - s } 
    else if tempIn == 2 && (posx m) < cols' then m{ posx = (posx m) + s } 
    else if tempIn == 8 && (posx m) >= 2 then m{ posx = (posx m) - s } 
    else m where
        tempIn = (input m)
        st = (stage m)
        (temp1, temp2, temp3) = if st == 5 then (generatePuzzle [] buffer [] 0 1 rows' 1 (randGen m))
            else ([0], [0], g)

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
handleInput (EventKey (SpecialKey KeyEnter) (Down) _ _) game = if (posx game) > 0 then game { grids = (changeN pos (grids game)), moves = tmoves+1 } else game where
    pos = (((toFloat (floor (posy game)) 0) - 1)*rows'+ (toFloat (floor (posx game)) 0)-1 )
    tmoves = (moves game)
handleInput (EventKey(Char '1') (Up)_ _)game =
    if stage game == 0 then game {stage = 1} 
    else if stage game == 1 then level11
    else if stage game == 2 then level21
    else if stage game == 3 then level31
    else game
handleInput (EventKey(Char '2') (Up)_ _)game =  
    if stage game == 0 then game {stage = 2} 
    else if stage game == 1 then level12
    else if stage game == 2 then level22
    else if stage game == 3 then level23
    else game
handleInput (EventKey(Char '3') (Up)_ _)game =  
    if stage game == 0 then game {stage = 3} 
    else if stage game == 1 then level13
    else if stage game == 2 then level23
    else if stage game == 3 then level33
    else game
handleInput (EventKey(Char '4') (Up)_ _)game =  
    if stage game == 0 then game {stage = 3} 
    else if stage game == 1 then level13
    else if stage game == 2 then level23
    else if stage game == 3 then level33
    else game
handleInput (EventKey(Char 'b') (Up)_ _)game =  
    if stage game == 0 then game {stage = 5} 
    else if stage game == 1 then game {stage = 0} 
    else if stage game == 2 then game {stage = 0} 
    else if stage game == 3 then game {stage = 0} 
    else game
handleInput _ game = game

fps :: Int
fps = 60

main :: IO ()
main = do
        grid0 <- loadBMP "./src/shapes/e0.bmp"
        play window background fps initState (renderState grid0) handleInput updateState



