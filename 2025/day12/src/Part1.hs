module Part1 (main) where

import Data.List.Split (splitOn)

parseRegion :: String -> ((Int, Int), [Int])
parseRegion txt = (dims, nShapes)
    where ns = words txt
          dims = ((\[a, b] -> (a, b)) . map read . splitOn "x" . init . head) ns
          nShapes = (map read . tail) ns

possRegion :: [Int] -> ((Int, Int), [Int]) -> Bool
possRegion shapeSizes ((d1, d2), nShapes) = goodSize && goodTiles
    where minSize = sum $ zipWith (*) shapeSizes nShapes
          regionArea = d1 * d2
          goodSize = minSize <= regionArea
          goodTiles = (sum nShapes * 9) <= regionArea

main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"

    let txtblocks = splitOn "\n\n" txt
    let shapetxt = init txtblocks
    let regiontxt = last txtblocks
    print shapetxt
    print regiontxt

    let shapeSizes = map (length . filter (== '#')) shapetxt
    print shapeSizes

    let regions = map parseRegion (lines regiontxt)
    print regions

    let goodRegions = filter (possRegion shapeSizes) regions
    print goodRegions
    print $ length goodRegions
