module Main where

import Data.Function (on)
import Data.List (maximumBy, nub)

data Matrix = Matrix {
  points :: [(Int, Int)],
  xMax :: Int,
  yMax :: Int
} deriving Show

parse :: String -> ([(Int, Int)], [Matrix -> Matrix])
parse s = (points, commands) where
  points = map makePoint . filter (/= "") . lines . takeWhile (/= 'f') $ s where
    makePoint :: String -> (Int, Int)
    makePoint s = read $ "(" ++ s ++ ")"
  commands = map parseFunc . lines . dropWhile (/= 'f') $ s where
    parseFunc s 
      | 'x' `elem` s = foldx n
      | 'y' `elem` s = foldy n
      where
        n = read . filter (`elem` ['0'..'9']) $ s :: Int

makeMatrix :: [(Int, Int)] -> Matrix
makeMatrix ps = Matrix ps xMax yMax where
  xMax = fst . maximumBy (compare `on` fst) $ ps
  yMax = snd . maximumBy (compare `on` snd) $ ps

pprint :: Matrix -> IO ()
pprint (Matrix ps xMax yMax) = mapM_ putStr 
  [if p `mod` x == xMax then c ++ "\n" else c | p <- [0..size], let c = if (p `mod` x, p `div` x) `elem` ps then "#" else "." ] where
    x = 1 + xMax
    y = 1 + yMax
    size = x * y - 1

foldx :: Int -> Matrix -> Matrix
foldx xFold (Matrix ps xMax yMax) = Matrix ps' xMax' yMax where
  xMax' = xFold
  ps' = nub $ map foldx' ps where
    foldx' :: (Int, Int) -> (Int, Int)
    foldx' (x, y)
      | x > xFold = (2 * xFold - x, y)
      | otherwise = (x, y)

foldy :: Int -> Matrix -> Matrix
foldy yFold (Matrix ps xMax yMax) = Matrix ps' xMax yMax' where
  yMax' = yFold
  ps' = nub $ map foldy' ps where
    foldy' :: (Int, Int) -> (Int, Int)
    foldy' (x, y)
      | y > yFold = (x, 2 * yFold - y)
      | otherwise = (x, y)

solution1 :: Matrix -> (Matrix -> Matrix) -> Int
solution1 mat f = length . points . f $ mat

solution2 :: Matrix -> [(Matrix -> Matrix)] -> Matrix
solution2 mat folds = origami mat
  where
    -- fold the folds!
    origami = foldr (\b a -> a . b) id folds

main :: IO ()
main = do
    (points, commands) <- parse <$> readFile "inputs.txt"
    print $ solution1 (makeMatrix points) (commands !! 0)
    let folded = solution2 (makeMatrix points) commands
    pprint folded
    --print $ commands