module Part2 where

import Data.Char

bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []


parseNums :: [String] -> [Integer] -> [[Integer]]
parseNums [] nums = [nums]
parseNums (h : t) nums | any isNumber h = parseNums t (read h : nums)
                       | otherwise = reverse nums : parseNums t []


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"
    print txt

    let ls = lines txt

    let numsTxt = foldr (zipWith (:)) (repeat []) (init ls)
    let nums = parseNums numsTxt []
    print nums

    let ops = map (\op -> if op == "+" then (+) else (*)) (split (last ls) ' ')

    let rs = zipWith foldl1 ops nums
    print rs
    print $ sum rs
