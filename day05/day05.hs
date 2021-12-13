module Main where

import Data.List (sort, group)
import Data.Char (isAlphaNum)
import Control.Arrow ((&&&))

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord)
data Line = Line Point Point deriving Show
type Array = [[(Point, Int)]] 

-- not needed since we derive Ord
--instance Eq Point where
--  (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2
  

parse = lines

-- given an input in the form x1,y1
-- read a string into a point
readPoint :: String -> Point
readPoint s = Point x y
  where
    tuple = break (==',') s
    x = read (fst tuple)
    y = read (tail (snd tuple)) 

-- given an input in the form x1,y1 -> x2,y2
-- read a string into a Line
readLine :: String -> Line
readLine s = Line (readPoint first) (readPoint second)
  where
    first = fst . break (==' ') $ s
    second = dropWhile (not . isAlphaNum) . snd . break (==' ') $ s

-- given two (x or y) coordinates, 
-- find all values between those points
steps :: Int -> Int -> [Int]
steps x1 x2 
  | x1 == x2 = [x1]
  | otherwise = x1 : steps (x1 + step) x2
  where
    step = signum (x2 -x1)

-- given a line, return a diagonal line between the points (if exists)
diagonal :: Line -> [Point]
diagonal (Line (Point x1 y1) (Point x2 y2))
  -- | x1 == y1 && x2 == y2 = []
  | abs xdiff /= abs ydiff = []
  | otherwise = take (step+1) $ (Point x1 y1) : diagonal (Line (Point (x1+xstep) (y1+ystep)) (Point x2 y2))
  where
    xdiff = (x2 - x1)
    ydiff = (y2 - y1)
    xstep = signum xdiff
    ystep = signum ydiff
    step = abs xdiff

-- given a Line, return a list of points on that line
drawLine :: Line -> [Point]
drawLine line@(Line (Point x1 y1) (Point x2 y2))
  -- p1 and p2 are the same point
  | x1 == x2 && y1 == y2 = [Point x1 y1] 
  -- vertical & horizontal lines
  | x1 == x2 = [Point x1 y' | y' <- y1 `steps` y2]
  | y1 == y2 = [Point x' y1 | x' <- x1 `steps` x2]
  -- diagonals
  | abs xdiff == abs ydiff = diagonal line 
  | otherwise = []
  where
    xdiff = (x2 - x1)
    ydiff = (y2 - y1)

dropDiagonals :: [Line] -> [Line]
dropDiagonals ls = filter p ls
  where
    p (Line (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

drawPoints :: [Line] -> [Point]
drawPoints = concat . map drawLine

-- given a list of things, count how many times they occur
countThings :: Ord a => [a] -> [(a, Int)]
countThings = map (head &&& length) . group . sort

-- given a list of points, count how many occur at least twice!
countDupes :: [Point] -> [(Point, Int)]
countDupes = filter p . countThings
  where
    p (point, count) = count >= 2

main :: IO ()
main = do
  sample <- readFile "sample.txt"
  inputs <- readFile "inputs.txt"
  print $ length . countDupes . drawPoints . dropDiagonals . map readLine . lines $ sample
  print $ length . countDupes . drawPoints . dropDiagonals . map readLine . lines $ inputs
  print $ length . countDupes . drawPoints . map readLine . lines $ sample
  print $ length . countDupes . drawPoints . map readLine . lines $ inputs


