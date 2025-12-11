module Part1 where

import Data.Char
import qualified Data.Vector as V

bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []

neighbors :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbors (r, c) rows cols =
    [(x, y)
    | x <- [r-1..r+1],
    y <- [c-1..c+1],
    not (x == r && y == c),
    within x 0 rows && within y 0 cols
    ]
    where within i l u = (i >= l) && (i < u)

type Matrix a = V.Vector (V.Vector a)

toMatrix :: [[a]] -> Matrix a
toMatrix = V.fromList . map V.fromList

canAccess :: (Int, Int) -> Matrix Char -> Int -> Int -> Bool
canAccess coord rollMat rows cols = adjrolls < 4
    where adjrolls = length $ filter (== '@') adjvals
          adjvals = map (\(x, y) -> (rollMat V.! x) V.! y) (neighbors coord rows cols)

main :: IO ()
main = do
    -- txt <- readFile "test.txt"
    txt <- readFile "input.txt"
    print txt

    let ls = lines txt
    print ls

    let rollMat = toMatrix ls
    print rollMat
    let rows = length rollMat
    let cols = length $ rollMat V.! 0
    print (rows, cols)

    let rolls = [(r, c) | r <- [0..rows-1], c <- [0..cols-1], (rollMat V.! r) V.! c == '@']
    let access = filter (\coord -> canAccess coord rollMat rows cols) rolls
    print access
    print $ length access
