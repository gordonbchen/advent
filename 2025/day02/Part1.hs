module Part1 where

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
isInvalid x = even xlen && (x == xhalf + (xhalf * xpow))
  where xlen = length $ show x
        xpow = 10 ^ (xlen `div` 2)
        xhalf = x `mod` xpow

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

