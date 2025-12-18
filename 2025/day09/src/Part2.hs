module Part2 (main) where

import Data.List (nub)

bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []


type Point = (Int, Int)

calcArea :: Point -> Point -> Int
calcArea (x, y) (x1, y1) = (abs (x - x1) + 1) * (abs (y - y1) + 1)

ins :: Int -> Int -> Int -> Bool
ins x1 x2 x3 = (x3 > min x1 x2) && (x3 < max x1 x2)

inside :: Point -> Point -> (Point, Point) -> Bool
inside (x1, y1) (x2, y2) ((x3, y3), (x4, y4)) = (inx && crossy) || (iny && crossx)
    where inx = all (ins x1 x2) [x3, x4]
          crossy = not $ all (>= max y1 y2) [y3, y4] || all (<= min y1 y2) [y3, y4]
          iny = all (ins y1 y2) [y3, y4]
          crossx = not $ all (>= max x1 x2) [x3, x4] || all (<= min x1 x2) [x3, x4]

goodPairs :: [Point] -> [(Point, Point)] -> [(Point, Point)]
goodPairs [] _ = []
goodPairs (h : t) sides = [(h, x) | x <- t, not $ any (inside h x) sides] ++ goodPairs t sides


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"
    let pts = (map ((\[x, y] -> (x, y)) . map (\x -> read x :: Int) . (`split` ',')) . lines) txt

    let sides = zip pts (tail pts)
    let pps = goodPairs pts sides

    let areas = map (uncurry calcArea) pps
    let (maxarea, bestpts) = maximum $ zip areas pps
    print maxarea
    print bestpts
