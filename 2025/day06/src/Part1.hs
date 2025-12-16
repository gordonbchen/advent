module Part1 where


bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []


mapApply :: [a -> b] -> [a] -> [b]
mapApply = zipWith (\f x -> f x)


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"
    print txt

    let ls = [split l ' ' | l <- lines txt]
    print ls

    let nums = map (map read) (init ls) :: [[Integer]]
    let ops = map (\op -> if op == "+" then (+) else (*)) (last ls)
    print nums

    let rs = foldl1 (mapApply . mapApply ops) nums
    print rs
    print $ sum rs
