module Part1 (main) where

import Control.Monad
import Control.Monad.State
import Lib

data Dir = L Int | R Int deriving (Show)

toDir :: String -> Dir
toDir ('L' : x) = L (read x :: Int)
toDir ('R' : x) = R (read x :: Int)

getNextPos :: Dir -> Int -> Int
getNextPos (L x) pos = (pos - x) `mod` 100
getNextPos (R x) pos = (pos + x) `mod` 100


turnDial :: Dir -> State Int Int
turnDial d = do
    pos <- get
    let nextPos = getNextPos d pos
    put nextPos
    return nextPos

main :: IO ()
main = do
    -- text <- readFile "test.txt"
    text <- readFile "input.txt"
    -- print text

    let ls = lines text
    let dirs = map toDir ls
    -- print dirs

    let poss = evalState (forM dirs turnDial) 50
    -- print poss

    let zs = length $ filter (== 0) poss
    print zs
