
module Day04 where

import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad
import Data.Foldable

data Game = Game Integer [Integer] [Board]
type Board = [[Square]]
type Square = Maybe Integer

input :: IO Game
input = parseInput . splitOn "\n\n" <$> readFile "inputs.txt"

sample :: IO Game
sample = parseInput . splitOn "\n\n" <$> readFile "sample.txt"

parseInput :: [String] -> Game
parseInput (x:xs) = Game 0 (parseDraws x) (map (parseBoard . lines) xs)
parseInput _ = error "invalid input"

parseDraws :: String -> [Integer]
parseDraws = map read . splitOn ","

parseBoard :: [String] -> Board
parseBoard = map $ map (return . read) . words

won :: Board -> Bool
won b = rowCheck b || colCheck b where
  rowCheck = any (all isNothing)
  colCheck = rowCheck . transpose

markBoard :: Integer -> Board -> Board
markBoard n = map (map (mfilter (/=n)))

takeTurn :: Game -> Game
takeTurn (Game _ [] bs) = Game undefined [] bs
takeTurn (Game _ (n:ns) bs) = Game n ns (map (markBoard n) bs)

boardScore :: Board -> Integer -> Integer
boardScore b n = n * sum (map (fromMaybe 0) $ concat b)

wonBoardScore :: Game -> Maybe Integer
wonBoardScore (Game n _ bs) = (`boardScore` n) <$> find won bs

solve1 :: Game -> Integer
solve1 = fromJust . asum . map wonBoardScore . iterate takeTurn

solve2 :: Game -> Integer
solve2 = solve1 . head . filter oneBoardLeft . map removeWon . iterate takeTurn
  where
    oneBoardLeft (Game _ _ bs) = length bs == 1
    removeWon (Game n ns bs) = Game n ns (filter (not . won) bs)

main :: IO ()
main = do
  input >>= print . solve1
  input >>= print . solve2
