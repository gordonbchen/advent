module Part2 where

import Data.Char
import Data.List (sortBy)

bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []

getMax' :: Int -> Int -> [Int] -> Int
getMax' l r [] = (l * 10) + r
getMax' l r (h : t) | (r * 10 + h) > (l * 10 + r) = getMax' r h t
                    | h > r = getMax' l h t
                    | otherwise = getMax' l r t

getMax :: String -> Int
getMax xs = getMax' 0 0 (map digitToInt xs)

main :: IO ()
main = do
    -- txt <- readFile "test.txt"
    txt <- readFile "input.txt"
    print txt

    let ls = lines txt
    print ls

    let maxs = map getMax ls
    print maxs

    print $ sum maxs
