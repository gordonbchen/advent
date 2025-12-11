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

eat :: [Char] -> Char -> [Char]
eat [] h = []
eat [x] h = [max x h]
eat (x1 : x2 : xs) h | x2 > x1 = x2 : xs ++ [h]
                     | otherwise = x1 : eat (x2 : xs) h

getMax :: [Char] -> Integer
getMax xs = read m :: Integer
    where m = foldl eat (take 12 xs) (drop 12 xs)

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
