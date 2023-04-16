module Day04 (day04) where

import Data.List (isSubsequenceOf)
import Data.List.Split (splitOn)

splitComa :: String -> (String, String)
splitComa s = case splitOn "," s of
                [x, y] -> (x, y)
                _      -> ("", "")

splitHyphen :: String -> (Int, Int)
splitHyphen s = case words (map (\c -> if c == '-' then ' ' else c) s) of
                [x, y] -> (read x, read y)
                _      -> (0, 0)

fullyContains :: ([Int], [Int]) -> Int
fullyContains (a, b)
    | isSubsequenceOf a b == True = 1
    | isSubsequenceOf b a == True = 1
    | otherwise = 0

overlap :: ([Int], [Int]) -> Int
overlap (a, b) = fromEnum (not (null [x | x <- a, elem x b]))

arr :: (Int, Int) -> [Int]
arr (a, b) = [a..b]

makeArr :: (String, String) -> ([Int], [Int])
makeArr (a, b) = (arr (splitHyphen a), arr (splitHyphen b))

day04a :: [String] -> Int
day04a [] = 0
day04a (x:xs) = fullyContains (makeArr (splitComa x)) + day04a xs

day04b :: [String] -> Int
day04b [] = 0
day04b (x:xs) = overlap (makeArr (splitComa x)) + day04b xs

day04 :: String -> IO()
day04 file = do
    input <- readFile file
    putStrLn "---------- DAY 04 ----------"
    print $ day04a $ parse input
    print $ day04b $ parse input
        where parse i = lines i