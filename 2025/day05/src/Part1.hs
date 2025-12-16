module Part1 where


bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []


type Range = (Integer, Integer)

parseRanges :: [String] -> [Range] -> ([Range], [String])
parseRanges ("" : t) ranges = (ranges, t)
parseRanges (h : t) ranges = parseRanges t ((l, u) : ranges)
    where [l, u] = map read $ split h '-'

parseInput :: String -> ([Range], [Integer])
parseInput txt = (ranges, nums)
    where (ranges, remLines) = parseRanges (lines txt) []
          nums = map read remLines


inRanges :: [Range] -> Integer -> Bool
inRanges ranges num = any (\(l, u) -> num >= l && num <= u) ranges


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"
    print txt

    let (ranges, nums) = parseInput txt
    print ranges
    print nums

    let goodNums = filter (inRanges ranges) nums
    print $ length goodNums
