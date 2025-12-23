module Part1 (main) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Loops
import qualified Data.HashSet as HS
import Data.Bits (xor)


bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []

parseLine :: String -> ([Int], [[Int]])
parseLine line = (target, map toBasis nums)
    where (t : n) = split line ' '
          target = [if c == '.' then 0 else 1 | c <- tail $ init t]
          nums = [map read $ split (tail $ init x) ',' | x <- init n]
          toBasis xs = [if i `elem` xs then 1 else 0 | i <- [0 .. length target - 1]]

toInt :: [Int] -> Int
toInt = foldl (\acc x -> acc * 2 + x) 0

perms :: Int -> [[Int]]
perms n = foldl (\acc _ -> (:) <$> [0, 1] <*> acc) [[0], [1]] [1..n-1]

solveLine :: String -> Int
solveLine line = fst minNs
    where (target, bases) = parseLine line
          inttarget = toInt target
          intbases = map toInt bases
          ps = perms $ length bases
          baseSums = map (foldl xor 0 . zipWith (*) intbases) ps
          ns = map sum ps
          minNs = minimum $ filter ((== inttarget) . snd) (zip ns baseSums)

main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"

    let ls = lines txt
    let presses = map solveLine ls
    print presses
    print $ sum presses
