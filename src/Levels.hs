module Levels where

import System.Random
import Graphics.Gloss

data PuzzleState = Game{
    grids :: [Int],
    crctConfig :: [Int],
    rows, cols :: Int,
    posx, posy :: Float,
    input :: Int,
    stage :: Float,
    caption :: [Char],
    moves :: Int,
    best :: Int,
    randGen :: StdGen,
    gridPics::[Picture]

}

level11, level12, level13, level21, level22, level23, level31, level32, level33:: PuzzleState
level11 = Game
    {
    grids = [6, 10, 9, 10, 0, 5, 9, 5, 12] ,
    crctConfig = [3, 10, 9, 5, 0, 5, 6, 10, 12] ,
    rows = 3,
    cols = 3,
    posx = 1.5,
    posy = 1.5,
    input = 0,
    stage = 1.1,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }
level12 = Game
    {
    grids = [3, 14, 9, 13, 15, 7, 6, 11, 12] ,
    crctConfig = [3, 11, 9, 13, 15, 7, 6, 11, 12] ,
    rows = 3,
    cols = 3,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 1.2,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }
level13 = Game
    {
    grids = [1, 0, 8, 10, 0, 10, 1, 0, 8] ,
    crctConfig = [1, 0, 1, 5, 0, 5, 4, 0, 4] ,
    rows = 3,
    cols = 3,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 1.3,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }
level21 = Game
    {
    grids = [6, 13, 5, 3, 5, 13, 7, 7, 13, 11, 7, 5, 3, 5 ,14, 12] ,
    crctConfig = [3, 11, 10, 9, 5, 7, 11, 13, 7, 14, 13, 5, 6, 10, 14, 12] ,
    rows = 4,
    cols = 4,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 2.1,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }
level22 = Game
    {
    grids = [0, 2, 1, 0, 1, 10, 10, 0, 12, 15, 15, 6, 0, 6, 6, 1] ,
    crctConfig = [0, 1, 1, 0, 1, 5, 5, 0, 6, 15, 15, 9, 0, 6, 12, 4] ,
    rows = 4,
    cols = 4,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 2.2,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }
level23 = Game
    {
    grids = [8, 8, 1, 8, 8, 5, 10, 8, 12, 3, 6, 6, 6, 10, 5, 6] ,
    crctConfig = [1, 1, 1, 1, 4, 5, 5, 4, 3, 12, 6, 9, 6, 10, 10, 12] ,
    rows = 4,
    cols = 4,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 2.3,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }
level31 = Game
    {
    grids = [12, 5, 5, 5, 6, 2, 8, 7, 2, 8, 8, 5, 15, 5, 2, 2, 8, 13, 2, 8, 9, 5, 5, 5, 3] ,
    crctConfig = [3, 10, 10, 10, 9, 4, 2, 11, 8, 4, 2, 10, 15, 10, 8, 1, 2, 14, 8, 1, 6, 10, 10, 10, 12] ,
    rows = 5,
    cols = 5,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 3.1,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }
level32 = Game
    {
    grids = [12, 5, 13, 13, 3, 14, 5, 11, 10, 10, 7, 14, 15, 11, 13, 5, 5, 7, 5, 14, 12, 11, 14, 5, 9] ,
    crctConfig = [3, 10, 11, 11, 9, 7, 10, 13, 13, 5, 5, 7, 11, 15, 14, 13, 5, 5, 7, 10, 13, 6, 14, 14, 10, 12] ,
    rows = 5,
    cols = 5,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 3.2,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }
level33 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..5], y <- [1..5]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..5], y <- [1..5]] ,
    rows = 5,
    cols = 5,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 3.3,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9,
    gridPics = []
    }