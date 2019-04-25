module Levels
    (level11, level12, level13,
    level21, level22, level23,
    level31, level32, level33,
    PuzzleState(..)
    ) where

import System.Random

rows = 4
cols = 4

data PuzzleState = Game{
grids :: [Int],
crctConfig :: [Int],
posx, posy :: Float,
input :: Int,
stage :: Float,
caption :: [Char],
moves :: Int,
best :: Int,
randGen :: StdGen
}

level11, level12, level13, level21, level22, level23, level31, level32, level33:: PuzzleState
level11 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1.5,
    posy = 1.5,
    input = 0,
    stage = 1.1,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }
level12 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 1.2,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }
level13 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 1.3,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }
level21 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 2.1,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }
level22 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 2.2,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }
level23 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 2.3,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }
level31 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 3.1,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }
level32 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 3.2,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }
level33 = Game
    {
    grids = [((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    crctConfig = [2*((x+y) `rem` 4)| x <- [1..rows], y <- [1..cols]] ,
    posx = 1,
    posy = 1,
    input = 0,
    stage = 3.3,
    caption = "",
    moves = 0,
    best = 0,
    randGen = mkStdGen 9
    }