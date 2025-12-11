module Part2 where

bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []

lineToRange :: String -> (Integer, Integer)
lineToRange line = (u, l)
    where (u : l : n) = map (\x -> read x :: Integer) $ split line '-'

isInvalid :: Integer -> Bool
isInvalid x = any (== xstr) ys
    where xstr = show x
          xlen = length xstr
          ys = [concat (replicate (xlen `div` n) (take n xstr)) | n <- [1 .. xlen `div` 2], xlen `mod` n == 0]


getInvalids :: (Integer, Integer) -> [Integer]
getInvalids (u, l) = filter isInvalid [u..l]


main :: IO ()
main = do
    -- txt <- readFile "test.txt"
    txt <- readFile "input.txt"
    print txt
    let lines = split (init txt) ','
    let ranges = map lineToRange lines
    print ranges

    let invalids = concatMap getInvalids ranges
    print invalids
    print $ sum invalids

