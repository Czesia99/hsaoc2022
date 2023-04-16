module Day03 (day03) where

import Data.Char (ord, isUpper)
import Data.List (intersect)
import Data.List.Split (chunksOf)

splitHalf :: String -> (String, String)
splitHalf str = (take half str, drop half str)
    where half = (length str + 1) `div` 2

common :: (String, String) -> String
common (a,b) = intersect a b

common3 :: (String, String, String) -> String
common3 (a, b, c) = foldl1 intersect [a, b, c]

count :: Char -> Int
count v
    | isUpper v = ord v - 38
    | otherwise = ord v - 96

day03a :: [String] -> Int
day03a [] = 0
day03a (x:xs) = count (head (common (splitHalf x))) + day03a xs

day03b :: [String] -> Int
day03b [] = 0
day03b [_] = 0
day03b [_, _] = 0
day03b (x:y:z:xs) = count (head (common3 (x,y,z))) + day03b xs

day03 :: String -> IO()
day03 file = do
    input <- readFile file
    putStrLn "---------- DAY 03 ----------"
    print $ day03a $ parse input
    print $ day03b $ parse input
        where parse i = lines i