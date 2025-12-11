module Part2 (main) where

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

getClicks :: Int -> Int -> Dir -> Int
getClicks pos nextPos (L x) = (x `div` 100) + (if (pos /= 0) && pos - (x `mod` 100) <= 0 then 1 else 0)
getClicks pos nextPos (R x) = (x `div` 100) + (if pos + (x `mod` 100) >= 100 then 1 else 0)

turnDial :: Dir -> State (Int, Int) (Int, Int)
turnDial d = do
    (pos, clicks) <- get
    let nextPos = getNextPos d pos
    let nextZs = clicks + getClicks pos nextPos d
    put (nextPos, nextZs)
    get

main :: IO ()
main = do
    -- text <- readFile "test.txt"
    text <- readFile "input.txt"
    -- print text

    let ls = lines text
    let dirs = map toDir ls
    print dirs

    let (poss, (pos, clicks)) = runState (forM dirs turnDial) (50, 0)
    print poss
    print clicks
