module Part1 (main) where

import Data.List (sort, sortOn, nub)
import Data.Ord (Down(..))
import Data.Array
import Control.Monad.State
import Control.Monad


bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []


dist :: [Integer] -> [Integer] -> Integer
dist xs ys = sum [(x - y) ^ 2 | (x, y) <- zip xs ys]

dists :: Array Int [Integer] -> [((Int, Int), Integer)]
dists arr = [((i, j), dist (arr ! i) (arr ! j)) | i <- [0..snd (bounds arr)], j <- [0..i-1]]


getUSHead :: Array Int (Maybe Int) -> Int -> Int
getUSHead unionSet idx = case unionSet ! idx of
    Nothing -> idx
    (Just nextIdx) -> getUSHead unionSet nextIdx

merge :: State (Array Int (Maybe Int), [((Int, Int), Integer)]) ()
merge = do
    (circuits, ds) <- get
    let ((u, v), _) = head ds
    let uhead = getUSHead circuits u
    let vhead = getUSHead circuits v
    if uhead == vhead
        then put (circuits, tail ds)
        else do
            let newCircuits = circuits // [(uhead, Just vhead)]
            put (newCircuits, tail ds)


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"

    let boxes = map (\l -> map read $ split l ',' :: [Integer]) $ lines txt
    print boxes

    let aboxes = listArray (0, length boxes - 1) boxes
    let ds = sortOn snd $ dists aboxes

    let circuits = listArray (0, length boxes - 1) (repeat (Nothing :: Maybe Int))

    let (newCircuits, newDs) = execState (replicateM 1000 merge) (circuits, ds)
    print newCircuits

    let heads = map (getUSHead newCircuits) [0..length boxes - 1]
    print heads

    let counts = map (\x -> length $ filter (== x) heads) $ nub heads
    print counts
    print $ (product . take 3 . sortOn Down) counts
