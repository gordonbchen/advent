module Part1 where

import Data.List (nub)
import Control.Monad
import Control.Monad.State


find :: String -> Char -> [Int]
find str target = [x | (x, c) <- zip [1..] str, c == target]

beamSplit :: [Int] -> State ([Int], Int) ()
beamSplit splitters = do
    (beams, splits) <- get
    let (beams', splits') = foldl (\(bs, n) b -> if b `elem` splitters then ([b-1,b+1]++bs, n+1) else (b:bs, n)) ([], 0) beams
    put (nub beams', splits' + splits)


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"
    let ls = lines txt

    let beams = find (head ls) 'S'
    print beams

    let splitters = map (`find` '^') (tail ls)
    print splitters

    let (newBeams, nSplits)  = execState (forM splitters beamSplit) (beams, 0)
    print nSplits
