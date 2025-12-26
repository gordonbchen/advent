module Part1 (main) where

import qualified Data.HashMap.Strict as HM

bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []


pathsToOut :: HM.HashMap String [String] -> String -> Integer
pathsToOut _ "out" = 1
pathsToOut devices d = sum $ map (pathsToOut devices) (HM.findWithDefault [] d devices)


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"

    let ls = map (\l -> (\(h : t) -> (init h, t)) $ split l ' ') $ lines txt
    let devices = HM.fromList ls
    print devices
    print $ pathsToOut devices "you"
