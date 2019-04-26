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

width, height, xoffset, yoffset :: Int
width = 1280
height = 960
xoffset = 300
yoffset = 50

window :: Display
window = InWindow "Infinite Loop" (width, height) (xoffset, yoffset)

background:: Color
background = black

gridSize ::Int
gridSize = 48

gridSize' :: Float
gridSize' = 48
-- | Generates one grid of puzzle
eachGrid :: Picture -> Float -> Float -> Int -> Color -> Picture
eachGrid pics x y a bgc = translate (x*gridSize'+x) (y*gridSize'+y) $ color bgc $ pics where

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs


--buffer = [0|x<-[1..cols]]

now = getCurrentTime
timezone = getCurrentTimeZone
g = mkStdGen 9
    {-do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    return (mkStdGen minute)-}

initState :: PuzzleState
initState = Game{
    grids = [],
    crctConfig = [1],
    rows = 5,
    cols = 5,
    posx = 1.5,
    posy = 1.5,
    input = 0,
    stage = 0,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 10,
    gridPics = []
}

toFloat :: Int -> Float ->Float
toFloat 0 a = a
toFloat a b = if a>0 then toFloat (a-1) (b+1) else toFloat (a+1) (b-1)

high = rectangleWire 48 48

-- | Generates whole puzzle
pRenderState :: [Picture] -> [Int] -> Float -> Float -> Float -> Float -> Float -> Float -> [Picture]
pRenderState pic [] x y r c px py = [ text "Moves"]
pRenderState pic (a:xs) x y r c px py = (if x == (toFloat (floor px) 0) && y == (toFloat (floor py) 0) then ([eachGrid (pic!!(a+5)) x y a green]++[eachGrid high x y a white]) else ([eachGrid (pic!!(a+5)) x y a black])) ++ (pRenderState pic xs (fst (getNextPos x y r c)) (snd (getNextPos x y r c)) r c px py)
-- | Function to load all screens
renderState :: [Picture] -> PuzzleState -> Picture
renderState a s =
    if tempStage == 0 then a!!0
    else if tempStage == 0.5 then a!!1
    else if tempStage == 1 || tempStage == 2 || tempStage == 3 then  a!!2
    else if tempStage == 4 then a!!3
    else if tempStage == 7 then a!!4
    else pictures((pRenderState a (grids s) 1 1 (toFloat (rows s) 0) (toFloat (cols s) 0) (posx s) (posy s))) where
        tempStage = (stage s)

generatePuzzle :: [Int] -> [Int] -> [Int] -> Int -> Float -> Float -> Float -> StdGen -> ([Int], [Int], StdGen)
generatePuzzle x y b d row mrow col g' = (puz, shufpuz, newgen) where
    (puz, newgen) = generateRow x y b d row mrow col g'
    shufpuz = shuffle puz newgen

checkSame:: [Int]->[Int]->Bool
checkSame [] [] = True
checkSame [] a = False
checkSame a [] = False 
checkSame (a:as) (b:bs) = if a==b then checkSame as bs else False

-- Make movement more smooth
-- | Takes an old state and gives a new state 
updateState :: Float -> PuzzleState -> PuzzleState
updateState s m =
    if st == 5 then m{crctConfig = temp1, grids = temp2 , randGen = temp3, stage = 3.4}
    else if (checkSame (grids m) (crctConfig m)) then m{stage = 4}
    else if (checkPuzzle (grids m) buffer [] 0 1 rows' 1 ) then m{stage = 4}
    else if tempIn == 1 && (posy m) < rows' then m{ posy = (posy m) + 3*s } 
    else if tempIn == 4 && (posy m) >= 2 then m{ posy = (posy m) - 3*s } 
    else if tempIn == 2 && (posx m) < cols' then m{ posx = (posx m) + 3*s } 
    else if tempIn == 8 && (posx m) >= 2 then m{ posx = (posx m) - 3*s } 
    else m where
        tempIn = (input m)
        st = (stage m)
        rows' = (toFloat (rows m) 0)
        cols' = (toFloat (cols m) 0)
        buffer = [0|x<-[1..(cols m)]]
        (temp1, temp2, temp3) = if st == 5 then (generatePuzzle [] buffer [] 0 1 rows' 1 (randGen m))
            else ([0], [0], g)

changeN :: Float -> [Int] -> [Int]
changeN _ [] = []
changeN 0 (a:as) = (trans a) : as
changeN i (a:as) = a : changeN (i-1) as

--stage 1 2 3 1.x 2.x 3.x are levels, 0 start page, 4 complted puzzle, 5 loading random puzzle
-- 6 loaded rand puzzle
-- Function takes the input from the user and updates the window accordingly
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
    pos = (((toFloat (floor (posy game)) 0)-1)*(toFloat (rows game) 0)+ (toFloat (floor (posx game)) 0)-1 )
    tmoves = (moves game)
handleInput (EventKey(Char 's') (Up)_ _)game =
    if stage game == 0 then game {stage = 0.5} 
    else game
handleInput (EventKey(Char '1') (Up)_ _)game =
    if stage game == 0.5 then game {stage = 1} 
    else if stage game == 1 then level11
    else if stage game == 2 then level21
    else if stage game == 3 then level31
    else game
handleInput (EventKey(Char '2') (Up)_ _)game =  
    if stage game == 0.5 then game {stage = 2} 
    else if stage game == 1 then level12
    else if stage game == 2 then level22
    else if stage game == 3 then level23
    else game
handleInput (EventKey(Char '3') (Up)_ _)game =  
    if stage game == 0.5 then game {stage = 3} 
    else if stage game == 1 then level13
    else if stage game == 2 then level23
    else if stage game == 3 then level33
    else game
handleInput (EventKey(Char 'b') (Up)_ _)game =  
    if stage game == 0.5 then game {stage = 5} 
    else if stage game == 1 then game {stage = 0} 
    else if stage game == 2 then game {stage = 0} 
    else if stage game == 3 then game {stage = 0} 
    else if stage game == 7 then game {stage = 0.5}
    else if stage game == 4 then game {stage = 0.5, grids = [], crctConfig = [1]}  
    else game
handleInput (EventKey(Char 'q') (Up)_ _)game =  
    if floor (stage game) == 1 then game {stage = 7} 
    else if floor (stage game) == 2 then game {stage = 7} 
    else if floor (stage game) == 3 then game {stage = 7} 
    else game
handleInput _ game = game

fps :: Int
fps = 60

main :: IO ()
main = do
        grid0 <- loadBMP "./src/shapes/e0.bmp"
        grid1 <- loadBMP "./src/shapes/e1.bmp"
        grid2 <- loadBMP "./src/shapes/e2.bmp"
        grid3 <- loadBMP "./src/shapes/e3.bmp"
        grid4 <- loadBMP "./src/shapes/e4.bmp"
        grid5 <- loadBMP "./src/shapes/e5.bmp"
        grid6 <- loadBMP "./src/shapes/e6.bmp"
        grid7 <- loadBMP "./src/shapes/e7.bmp"
        grid8 <- loadBMP "./src/shapes/e8.bmp"
        grid9 <- loadBMP "./src/shapes/e9.bmp"
        grid10 <- loadBMP "./src/shapes/e10.bmp"
        grid11 <- loadBMP "./src/shapes/e11.bmp"
        grid12 <- loadBMP "./src/shapes/e12.bmp"
        grid13 <- loadBMP "./src/shapes/e13.bmp"
        grid14 <- loadBMP "./src/shapes/e14.bmp"
        grid15 <- loadBMP "./src/shapes/e15.bmp"
        startp <- loadBMP "./src/shapes/startpage.bmp"
        worldsp <- loadBMP "./src/shapes/world_screen.bmp"
        levelsp <- loadBMP "./src/shapes/level_screen.bmp"
        donep <- loadBMP "./src/shapes/gameOver_screen.bmp"
        quitp <- loadBMP "./src/shapes/GiveUp_screen.bmp"
        let pics = [startp]++[worldsp]++[levelsp]++[donep]++[quitp] ++ [grid0]++[grid1]++[grid2]++[grid3]++[grid4]++[grid5]++[grid6]++[grid7]++[grid8]++[grid9]++[grid10]++[grid11]++[grid12]++[grid13]++[grid14]++[grid15]
        play window background fps initState (renderState pics) handleInput updateState



