module Day06 (day06) where

import Data.List

containsDuplicate :: String -> Bool
containsDuplicate str = length (nub str) /= length str

-- findSignal :: String -> Int
-- findSignal [] = 0
-- findSignal str
--     | containsDuplicate (take 4 str) == True = 1 + findSignal (tail str)
--     | otherwise = 0
-- findSignal _ = 0

-- findMessages :: String -> Int
-- findMessages [] = 0
-- findMessages str
--     | containsDuplicate (take 14 str) == True = 1 + findMessages (tail str)
--     | otherwise = 0
-- findMessages _ = 0

findSignal' :: String -> Int -> Int
findSignal' [] n = 0
findSignal' str n
    | containsDuplicate (take n str) == True = 1 + (findSignal' (tail str) n)
    | otherwise = 0
findSignal' _ n = 0

day06a :: String -> Int
day06a input = (findSignal' input 4) + 4

day06b :: String -> Int
day06b input = (findSignal' input 14) + 14

day06 :: String -> IO()
day06 file = do
    input <- readFile file
    putStrLn "---------- DAY 06 ----------"
    print $ day06a input
    print $ day06b input