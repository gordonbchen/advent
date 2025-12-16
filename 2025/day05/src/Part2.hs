module Part2 where


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

mergeRanges :: [Range] -> Range -> [Range]
mergeRanges [] r = [r]
mergeRanges ((l, u) : t) (newl, newu)
    | newl > u || newu < l = (l, u) : mergeRanges t (newl, newu)
    | otherwise = foldl mergeRanges ((l, u) : t) (low ++ high)
    where low = [(newl, l-1) | newl < l]
          high = [(u+1, newu) | newu > u]


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"
    print txt

    let (ranges, _) = parseInput txt
    print ranges

    let merged = foldl mergeRanges [] ranges
    print merged

    let nmerged = foldl (\acc (l, u) -> acc + (u - l + 1)) 0 merged
    print nmerged
