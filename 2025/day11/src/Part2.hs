module Part2 (main) where

import qualified Data.HashMap.Strict as HM
import Control.Monad.State
import Control.Monad

bufToStr :: [Char] -> [String]
bufToStr buf = [reverse buf | not (null buf)]

splitBuf :: [Char] -> String -> Char -> [String]
splitBuf buf [] _ = bufToStr buf
splitBuf buf (h : t) delim | h == delim = bufToStr buf ++ splitBuf [] t delim
                           | otherwise = splitBuf (h : buf) t delim

split :: String -> Char -> [String]
split = splitBuf []


nPaths :: HM.HashMap String [String] -> String -> String -> State (HM.HashMap (String, String) Integer) Integer
nPaths _ s d | s == d = return 1
nPaths devices s d = do
    cache <- get
    case HM.lookup (s, d) cache of
        Just x -> return x
        Nothing -> do
            ns <- forM (HM.findWithDefault [] s devices) (\x -> nPaths devices x d)
            let n = sum ns
            modify' (HM.insert (s, d) n)
            return n

totalPaths :: HM.HashMap String [String] -> [String] -> State (HM.HashMap (String, String) Integer) Integer
totalPaths ds [h, t] = nPaths ds h t
totalPaths ds (a : b : c) = do
    n <- nPaths ds a b
    n1 <- totalPaths ds (b : c)
    return $ n * n1

main :: IO ()
main = do
    -- txt <- readFile "inputs/test2.txt"
    txt <- readFile "inputs/input.txt"

    let ls = map (\l -> (\(h : t) -> (init h, t)) $ split l ' ') $ lines txt
    let devices = HM.fromList ls
    print devices
    let cache = HM.fromList []
    let (fftDac, newCache) = runState (totalPaths devices ["svr", "fft", "dac", "out"]) cache
    let dacFft = evalState (totalPaths devices ["svr", "dac", "fft", "out"]) newCache
    print $ fftDac + dacFft
