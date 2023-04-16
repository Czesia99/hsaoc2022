module Day02 (day02) where

data Shape = Rock
            | Paper
            | Scissors
    deriving (Show, Eq)

data Out = Win
            | Loose
            | Draw
    deriving (Show, Eq)

shape :: Char -> Shape
shape c
    | c == 'A' || c == 'X' = Rock
    | c == 'B' || c == 'Y' = Paper
    | otherwise = Scissors

shapeInt :: Shape -> Int
shapeInt s
    | s == Rock = 1
    | s == Paper = 2
    | otherwise = 3

out :: Char -> Out
out c
    | c == 'X' = Loose
    | c == 'Y' = Draw
    | otherwise = Win

rr :: (Shape, Shape) -> Int
rr (a, b)
    | a == b = 3 + shapeInt b
    | a == Rock && b == Paper = 6 + shapeInt b
    | a == Paper && b == Scissors = 6 + shapeInt b
    | a == Scissors && b == Rock = 6 + shapeInt b
    | otherwise = 0 + shapeInt b

rr' :: (Char, Char) -> Int
rr' (a, b)
    | out b == Draw = 3 + shapeInt (shape a)
    | out b == Win && a == 'A' = 6 + shapeInt Paper
    | out b == Win && a == 'B' = 6 + shapeInt Scissors
    | out b == Win && a == 'C' = 6 + shapeInt Rock
    | out b == Loose && a == 'A' = 0 + shapeInt Scissors
    | out b == Loose && a == 'B' = 0 + shapeInt Rock
    | out b == Loose && a == 'C' = 0 + shapeInt Paper
    | otherwise = 0

parse :: String -> [(Char, Char)]
parse input = [x | x <- map tuple (lines input)]
    where
        tuple [x,_,y] = (x,y)

day02a :: [(Char, Char)] -> Int
day02a [] = 0
day02a ((a,b):xs) = rr (shape a, shape b) + day02a xs

day02b :: [(Char, Char)] -> Int
day02b [] = 0
day02b (x:xs) = rr' x + day02b xs

day02 :: String -> IO()
day02 file = do
    input <- readFile file
    putStrLn "---------- DAY 02 ----------"
    print $ day02a $ parse input
    print $ day02b $ parse input