module Part1 (main) where

bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []


calcAreas :: [(Int, Int)] -> [Int]
calcAreas [] = []
calcAreas (h : t) = map (calcArea h) t ++ calcAreas t
    where calcArea (x, y) (x1, y1) = (abs (x - x1) + 1) * (abs (y - y1) + 1)


main :: IO ()
main = do
    -- txt <- readFile "inputs/test.txt"
    txt <- readFile "inputs/input.txt"

    let pts = (map ((\[x, y] -> (x, y)) . map (\x -> read x :: Int) . (`split` ',')) . lines) txt
    print pts

    print $ (maximum . calcAreas) pts
