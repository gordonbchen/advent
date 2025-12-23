module Part2 (main) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Loops
import qualified Data.HashSet as HS


bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []

parseLine :: String -> ([Int], [[Int]])
parseLine line = (target, bases)
    where (_ : n) = split line ' '
          target = [read x :: Int | x <- ((`split` ',') . tail . init . last) n]
          nums = [(map read . (`split` ',') . tail . init) x | x <- init n] :: [[Int]]
          -- bases = map (toInt . toBasis) nums
          bases = map toBasis nums
          toBasis xs = [if i `elem` xs then 1 else 0 | i <- [0 .. length target - 1]]
          -- toInt = foldl (\acc x -> acc * 2 + x) 0

dStep :: [Int] -> [[Int]] -> State (HS.HashSet [Int]) Int
dStep target bases = do
    vs <- get
    let possVs = zipWith (+) <$> bases <*> HS.toList vs
    let newVs = filter (and . zipWith (>=) target) possVs
    put $ HS.fromList newVs
    return 1

dCont :: [Int] -> State (HS.HashSet [Int]) Bool
dCont target = do
    vs <- get
    return $ (not . HS.member target) vs


solveLine :: String -> Int
solveLine line = n + 1
    where (target, bases) = parseLine line
          n = sum $ evalState (whileM (dCont target) (dStep target bases)) (HS.fromList bases)

main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"

    let ls = lines txt
    let presses = map solveLine ls
    print presses
    print $ sum presses
