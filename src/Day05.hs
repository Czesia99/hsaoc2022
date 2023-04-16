module Day05 (day05) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char()
import Data.Either()
import Data.Char

digitChar :: Parser Char
digitChar = satisfy isDigit

parseCrate :: Parser Char
parseCrate = do
  _ <- (char '[') <|> (char ' ')
  _ <- notFollowedBy digitChar
  c <- satisfy (\x -> isAlpha x || x == ' ')
  _ <- (char ']') <|> (char ' ')
  _ <- space <|> endOfLine
  pure c

parseCrates :: Parser [Char]
parseCrates = many parseCrate

parseInstructions :: Parser [(Int, Int, Int)]
parseInstructions = many1 parseInstruction

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseInstruction :: Parser (Int, Int, Int)
parseInstruction = do
  _ <- string "move "
  x <- parseNumber
  _ <- string " from "
  y <- parseNumber
  _ <-string " to "
  z <- parseNumber
  _ <- endOfLine
  pure (x, y, z)

parseInput :: Parser ([[Char]], [(Int, Int, Int)])
parseInput = do
  array <- many parseCrates
  skipMany (digitChar <|> space)
  skipMany space
  instructions <- parseInstructions
  eof
  pure (array, instructions)

day05 :: String -> IO()
day05 file = do
    input <- readFile file
    putStrLn "---------- DAY 05 ----------"
    case runParser parseInput () "" input of
      Left err -> putStrLn $ show err
      Right result -> print result