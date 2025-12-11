{-# LANGUAGE TupleSections #-}

module Part2 (main) where

import Data.Array
import Data.Char
import Control.Monad.State
import Control.Monad
import qualified Data.Set as S

getAdj :: Int -> Int -> (Int, Int) -> [(Int, Int)]
getAdj rows cols (r, c) =
    [(x, y)
    | x <- [r-1..r+1],
    y <- [c-1..c+1],
    not (x == r && y == c),
    within x 0 rows && within y 0 cols
    ]
    where within i l u = (i >= l) && (i < u)

getAdjRolls :: (Eq a) => Matrix a -> a -> (Int, Int) -> [(Int, Int)]
getAdjRolls mat t = filter (\(r, c) -> mat ! (r, c) /= t) . getAdj rows cols
    where (rows, cols) = getMatDims mat

type Matrix = Array (Int, Int)

getMatDims :: Matrix a -> (Int, Int)
getMatDims mat = (rows, cols)
    where ((_,_), (rowmax, colmax)) = bounds mat
          rows = rowmax + 1
          cols = colmax + 1

toAdjMat :: Matrix Char -> Matrix Int
toAdjMat mat = listArray (bounds mat) adjMat
    where
        (rows, cols) = getMatDims mat
        getNAdjRolls = length . getAdjRolls mat '.'
        adjMat = [if mat ! (r, c) /= '@' then 0 else getNAdjRolls (r, c) | r <- [0..rows-1], c <- [0..cols-1]]

reduceMat :: State (S.Set (Int, Int), Matrix Int) ()
reduceMat = do
    (coords, mat) <- get
    case S.minView coords of
        Nothing -> return ()
        Just (coord, t) -> do
            if (mat ! coord) < 4
                then do
                    let mat1 = mat // [(coord, 0)]
                    let (rows, cols) = getMatDims mat
                    let adj = getAdjRolls mat1 0 coord
                    let mat2 = accum (\old i -> max 0 $ old - i) mat1 $ map (, 1) adj
                    put (t `S.union` S.fromList adj, mat2)
                    -- put (t, mat2)
                    reduceMat
                else do
                    put (t, mat)
                    reduceMat

printMat :: (Show a) => Matrix a -> IO ()
printMat mat = do
    let (rows, cols) = getMatDims mat
    forM_ [(r, c) | r <- [0..rows-1], c <- [0..cols-1]] (printCoord mat cols)

printCoord :: (Show a) => Matrix a -> Int -> (Int, Int) -> IO ()
printCoord mat cols coord = do
    putStr $ show (mat ! coord)
    when (snd coord == cols - 1) $ putChar '\n'


main :: IO ()
main = do
    -- txt <- readFile "test.txt"
    txt <- readFile "input.txt"
    let ls = lines txt

    let rows = length ls
    let cols = length $ head ls
    let matrix = listArray ((0, 0), (rows - 1, cols - 1)) (concat ls)
    printMat matrix

    let adjMat = toAdjMat matrix
    printMat adjMat

    let rolls = S.fromList [(r, c) | r <- [0..rows-1], c <- [0..cols-1], matrix ! (r, c) == '@']
    putStrLn ""
    let (_, mat) = execState reduceMat (rolls, adjMat)
    printMat mat

    print $ length $ filter (\coord -> mat ! coord == 0) (S.toList rolls)
