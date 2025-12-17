module Part2 (main) where

import Data.List (nub)
import Data.Array
import Control.Monad
import Control.Monad.State

type Arr = Array Int Int

updateBeam :: Arr -> Arr -> Int -> Arr
updateBeam splits beams beam
    | splits ! beam == 1 = beams // ((beam, 0) : beamLeft ++ beamRight)
    | otherwise = beams
    where nbeams = beams ! beam
          beamLeft = [(beam-1, beams ! (beam-1) + nbeams) | beam-1 >= 0]
          beamRight = [(beam+1, beams ! (beam+1) + nbeams) | beam+1 <= snd (bounds beams)]

beamSplit :: Arr -> State Arr ()
beamSplit splits = do
    beams <- get
    let newBeams = foldl (updateBeam splits) beams ([i | (i, x) <- zip [0..] (elems beams), x /= 0])
    put newBeams


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"
    let ls = lines txt

    let (beams : dsplits) = map (map (\c -> if c == '.' then 0 else 1)) ls
    let splits = filter (elem 1) dsplits
    let abeams = listArray (0, length beams - 1) beams
    let asplits = map (\x -> listArray (0, length x - 1) x) splits
    print $ elems abeams
    print $ map elems asplits

    let newBeams = execState (forM asplits beamSplit) abeams
    print $ elems newBeams
    print $ sum (elems newBeams)
