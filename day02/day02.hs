{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Char (toUpper, toLower)

data Command = Forward Int | Down Int | Up Int deriving (Show, Read)
data Position = Position {
  horizontal :: Int,
  depth :: Int,
  aim :: Int
} deriving (Show)

origin :: Position
origin = Position 0 0 0

distance :: Position -> Int
distance (Position x y _) = x * y

capitalize :: String -> String
capitalize (x:xs) = toUpper x : map toLower xs 

parse :: String -> [Command]
parse = map read . map capitalize . lines

move1 :: Command -> Position -> Position
move1 (Forward m) (Position h d a) = Position {horizontal = h + m, depth = d, aim = a}
move1 (Down m)    (Position h d a) = Position {horizontal = h, depth = d + m, aim = a}
move1 (Up m)      (Position h d a) = Position {horizontal = h, depth = d - m, aim = a}

move2 :: Command -> Position -> Position
move2 (Forward m) (Position h d a) = Position {horizontal = h + m, depth = d + m * a, aim = a}
move2 (Down m)    (Position h d a) = Position {horizontal = h, depth = d, aim = a + m}
move2 (Up m)      (Position h d a) = Position {horizontal = h, depth = d, aim = a - m}


-- given a list of commands, generate a function that takes a Position 
-- and returns the Position after executing all commands
collapseMoves :: (Command -> Position -> Position) -> [Command] -> (Position -> Position) 
collapseMoves f = foldr collapse acc where
  -- accumulator is the null move function, so takes a position and goes nowhere
  acc :: Position -> Position
  --acc p = p
  acc = id
  collapse :: Command -> (Position -> Position) -> (Position -> Position)
  collapse command acc = acc . f command

main :: IO ()
main = do
  inputs <- readFile "inputs.txt"
  let newPosition1 = collapseMoves move1 (parse inputs) origin
  let distanceTravelled1 = distance newPosition1
  putStrLn $ "PART 1 -------"
  putStrLn $ "Your final position is " ++ show newPosition1
  putStrLn $ "Total distance travelled is " ++ show distanceTravelled1
  let newPosition2 = collapseMoves move2 (parse inputs) origin
  let distanceTravelled2 = distance newPosition2
  putStrLn $ "PART 2 -------"
  putStrLn $ "Your final position is " ++ show newPosition2
  putStrLn $ "Total distance travelled is " ++ show distanceTravelled2
