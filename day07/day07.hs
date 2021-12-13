module Main where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

sample :: [Int]
sample = [16,1,2,0,4,2,7,1,2,14]

parse :: String -> [Int]
parse s = read $ "[" ++ s ++ "]"

-- given a position and a list of positions, calculate the total cost of moving all elements to that position
cost :: Int -> [Int] -> Int
cost n = sum . map (abs . ((-) n ))

crabDistance :: Int -> Int
crabDistance n = n * (n + 1) `div` 2

cost' :: Int -> [Int] -> Int
cost' n = sum . map crabDistance . map (abs . ((-) n ))

costList :: (Int -> [Int] -> Int) -> [Int] -> [Int]
costList f xs = map ($ xs) fs
    where
        fs = map f ps
        ps = [0 .. maximum xs]

-- find the index of the lowest cost in the cost list
bestPosition :: [Int] -> Int
bestPosition xs = fromJust $ elemIndex (minimum (costList cost xs)) (costList cost xs)

solution1 :: [Int] -> Int
solution1 = minimum . costList cost

solution2 :: [Int] -> Int
solution2 = minimum . costList cost'

main :: IO ()
main = do
    inputs <- readFile "inputs.txt"
    print . solution1 . parse $ inputs
    print . solution2 . parse $ inputs
