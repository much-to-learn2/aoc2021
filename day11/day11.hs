module Main where

import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Data.List (sort, group)

type Matrix = [[Maybe Int]]

testCase = "5483143223\n\
\2745854711\n\
\5264556173\n\
\6141336146\n\
\6357385478\n\
\4167524645\n\
\2176841721\n\
\6882881134\n\
\4846848554\n\
\5283751526"

testCase' = "11111\n\
\19991\n\
\19191\n\
\19991\n\
\11111"

-- for some reason, (map . map) Just . digitToInt . lines wasn't working... but this does :/
digitToMaybeInt :: Char -> Maybe Int
digitToMaybeInt = Just . digitToInt

--parse :: String -> Matrix
parse = (map . map) digitToMaybeInt . lines

--helper function to replace element n in a list
--not super efficient - Sequence would be better
replace :: Maybe Int -> Int -> Int -> Matrix -> Matrix
replace n x y mat = replace' x (replace' y n (mat !! x)) mat
  where
    replace' :: Int -> a -> [a] -> [a]
    replace' _ _ [] = []
    replace' idx n (x:xs) 
      | idx == 0 = n:xs
      | otherwise = x:replace' (idx-1) n xs

--simply add 1 to all elements of our matrix - give it the SUCC
incrementMatrix :: Matrix -> Matrix
incrementMatrix = (fmap . fmap . fmap) succ

--increment all spaces next to a given space by 1
incrementNeighbors :: Int -> Int -> Matrix -> Matrix
incrementNeighbors x y mat =
    -- replace current element with NOthing, since we only do this if an octo flashed
    (replace Nothing x y) . 

    -- up 
    (replace (succ' $ mat !! (x-1) !! y) (x-1) y) . 
    -- down
    (replace (succ' $ mat !! (x+1) !! y) (x+1) y) .
    -- left
    (replace (succ' $ mat !! x !! (y-1)) x (y-1)) .
    -- right
    (replace (succ' $ mat !! x !! (y+1)) x (y+1)) .
    -- top left
    (replace (succ' $ mat !! (x-1) !! (y-1)) (x-1) (y-1)) .
    -- top right
    (replace (succ' $ mat !! (x-1) !! (y+1)) (x-1) (y+1)) .
    -- bottom left
    (replace (succ' $ mat !! (x+1) !! (y-1)) (x+1) (y-1)) .
    -- bottom right
    (replace (succ' $ mat !! (x+1) !! (y+1)) (x+1) (y+1))
    $ mat
    where
        succ' = fmap succ

-- flash is just incrementNeighbors, but only applied if the coordinate is > 9
flash :: Int -> Int -> Matrix -> Matrix
flash x y mat
  | fromMaybe 0 (mat !! x !! y) > 9 = incrementNeighbors x y mat
  | otherwise = mat

-- this function accepts a pair and returns a function going from matrix a to matrix b
temp :: (Int, Int) -> Matrix -> Matrix
temp = uncurry flash

-- this will generate a list of pairs that represent the coordinates of our matrix
--pairs :: Matrix -> [(Int, Int)]
pairs mat = [(x, y) | x <- [0..(length mat - 1)], y <- [0..((length . head) mat - 1)]]

-- given a matrix, apply `flash` to all elements once
flashAll :: Matrix -> Matrix
flashAll mat = foldl f id (pairs mat) $ mat
  where
      f a b = a . (uncurry flash b)

-- given a matrix, return a Bool indicating if we need to continue propagating
needsToPropagate :: Matrix -> Bool
needsToPropagate = not . null . filter p . concat
  where
      p (Just x) = x > 9
      p _ = False

-- given a matrix, return a Bool indicating if everyone just flashed
allFlashed :: Matrix -> Bool
allFlashed mat = all id . map justFlashed . concat $ mat
  where
      justFlashed (Just 0) = True
      justFlashed _ = False


-- given a matrix that has already been incremented, apply `flashAll` until there are no more octopi that need incrementing
propagate :: Matrix -> Matrix
propagate mat
  | needsToPropagate mat = propagate . flashAll $ mat
  | otherwise = mat

-- given a matrix, apply a single `step`
step :: Matrix -> Matrix
step = propagate . incrementMatrix

--how many octos have flashed this step?
countFlashes :: Matrix -> Int
countFlashes = length . filter p . concat
  where
      p Nothing = True
      p _ = False

--any octos that flashed should be reset to 0
resetOctos :: Matrix -> Matrix
resetOctos = (map . map) reset where
    reset Nothing = Just 0
    reset x = x

-- given a matrix and a count of flashes so far, apply a step and add that to the flash count
step' :: (Int, Matrix) -> (Int, Matrix)
step' (n, mat) = (n', mat')
  where
      mat' = resetOctos . step $ mat
      n' = n + countFlashes (step mat)

-- given a number of steps to apply and a matrix, return (flashes, Matrix)
applySteps :: Int -> Matrix -> (Int, Matrix)
applySteps n mat = head . reverse . (take (n+1)) $ (iterate step' (0, mat))

-- how many flashes after 100 steps?
solution1 :: Matrix -> Int
solution1 = fst . applySteps 100

-- what is the step number where all octos flash?
untilAllFlash mat = go (0, (0, mat))
  where
      go (n, (flashes, mat))
        | allFlashed mat = n
        | otherwise = go (n+1, (step' (n, mat)))

solution2 :: Matrix -> Int
solution2 = untilAllFlash


main :: IO ()
main = do
    return ()
    inputs <- parse <$> readFile "inputs.txt"
    print . solution1 $ inputs
    print . solution2 $ inputs
