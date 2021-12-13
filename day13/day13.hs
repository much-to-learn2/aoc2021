module Main where

type Matrix = [[Int]]


-- these are helper functions which calculate the new value of the coordinate being folded
foldx y t
  | y <= t = y
  | y > t = 2 * t - y

foldy x t
  | x <= t = x
  | x > t = 2 * t - x



main :: IO ()
main = do
    return ()
