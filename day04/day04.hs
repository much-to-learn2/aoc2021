module Main where

import Data.List (transpose, sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

data Space a = Marked a | Unmarked a deriving (Show)
type Board = [[Space Int]]
data BoardState = BoardState {
  board :: Board,
  isWon :: Bool,
  winningDraw :: Maybe Int,
  score :: Maybe Int,
  counter :: Int
} deriving Show

isMarked :: Space a -> Bool
isMarked (Marked _) = True
isMarked _ = False

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where (w, s'') = break p s'

parseHead :: String -> [Int]
parseHead = map read . split (==',') . head . lines

parseTail :: String -> [Board]
parseTail = makeBoard . stack5 . map (map read) . concat . map (map words) . foldList . tail . lines
  where
    foldList :: [String] -> [[String]]
    f :: [[String]] -> String -> [[String]]
    f acc "" = acc
    f (acc:accs) x = [x]:acc:accs
    f _ x = [[x]]
    foldList xs = foldl f [] xs

    stack5 :: [[Int]] -> [[[Int]]]
    stack5 [] = []
    stack5 xs = take 5 xs : stack5 (drop 5 xs)

    makeBoard :: [[[Int]]] -> [Board]
    makeBoard = map (map (map Unmarked))

-- given a board and an Int, play a turn
playTurn :: Int -> Board -> Board
playTurn n b = map (map (markSpace n)) b
  where
    markSpace :: Int -> Space Int -> Space Int
    markSpace _ (Marked s) = Marked s
    markSpace n (Unmarked s)
      | n == s = Marked s
      | otherwise = Unmarked s

-- given a board and Int, calculate the winning score
boardScore :: Int -> Board -> Int
boardScore n b = n * (sum $ map spaceScore (concat b) )
  where
    spaceScore :: Space Int -> Int
    spaceScore (Marked n) = 0
    spaceScore (Unmarked n) = n

-- determine if a boards current configuration is a bingo
isBingo :: Board -> Bool
isBingo board = any (==True) rows || any (==True) cols
  where
    rows = map (all isMarked) board
    cols = map (all isMarked) (transpose board)

playAllDraws :: [Int] -> [Board] -> [BoardState]
playAllDraws _ [] = []
playAllDraws draws@(x:xs) boards@(b:bs) = foldl playTurn' initialState draws : playAllDraws draws bs
  where
    initialState = BoardState {board = b, isWon = False, winningDraw = Nothing, score = Nothing, counter = (-1)}
    playTurn' :: BoardState -> Int -> BoardState
    playTurn' (BoardState b w d s c) n 
      | w = BoardState b w d s c 
      | isBingo (playTurn n b) = BoardState (playTurn n b) True (Just n) (Just (boardScore n (playTurn n b))) c
      | otherwise = BoardState (playTurn n b) w d s (c+1)

-- given a list of board states, find the first winner
firstWinner :: [BoardState] -> [BoardState]
firstWinner = sortBy ordering where
  ordering a b 
    | counter a > counter b = GT
    | counter a < counter b = LT
    | otherwise = EQ

-- comparing has type Ord a => (b -> a) -> a -> a -> Ordering,
-- where (b -> a) is a function to go from the thing you WANT To compare
-- to a thing that you CAN compare (like, for example, an Int)
-- so in this case, the function is counter (BoardState -> Int)
firstWinner' :: [BoardState] -> Int
firstWinner' = fromJust . score . head . sortBy (comparing counter)

lastWinner :: [BoardState] -> Int
lastWinner = fromJust . score . head . reverse . sortBy (comparing counter) . filter isWon

 
main :: IO ()
main = do
  inputs <- readFile "inputs.txt"
  let commands = parseHead inputs
      boards = parseTail inputs
  print . firstWinner' . playAllDraws commands $ boards
  print . lastWinner . playAllDraws commands $ boards
  return ()

  
