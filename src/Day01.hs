module Day01 (day01) where

import Data.List(sort)
import Data.List.Split

calories :: [[Int]] -> [Int]
calories i = map sum i

day01a :: [Int] -> Int
day01a cal = maximum cal

day01b :: [Int] -> Int
day01b cal = sum $ take 3 $ reverse (sort cal)

day01 :: String -> IO()
day01 file = do
    input <- readFile file
    putStrLn "---------- DAY 01 ----------"
    print $ day01a $ parse input
    print $ day01b $ parse input
        where parse input = calories $ map (map read . lines) (splitOn "\n\n" input)