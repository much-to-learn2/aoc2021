module Day03 where

import Data.List (group, sort, maximumBy, minimumBy, transpose, foldl')
import Data.Function (on)
import Debug.Trace

sample = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

bin2dec :: Int -> Int
bin2dec 0 = 0
bin2dec i = 2 * bin2dec (div i 10) + (mod i 10)

-- given a list, find the most common element
mode :: Ord a => [a] -> a
mode list = fst . maximumBy (compare `on` snd)  $ map (\x -> (head x, length x)) . group . sort $ list

-- given a list, find the least common element
antimode :: Ord a => [a] -> a
antimode list = fst . minimumBy (compare `on` snd)  $ map (\x -> (head x, length x)) . group . sort $ list

-- gamma rate - most common bit
gamma :: [String] -> Int
gamma xs = bin2dec . read . map mode $ transpose xs

-- epsilon rate - least common bit
epsilon :: [String] -> Int
epsilon xs = bin2dec . read . map antimode $ transpose xs

-- oxygen
oxygen :: Int -> [String] -> [String]
oxygen _ [x] = [x]
oxygen n xs = filter p xs
  where
    bit = mode $ (transpose xs) !! n
    p x = (x !! n) == bit 

oxygen' :: [String] -> Int -> [String]
oxygen' [x] _ = [x]
oxygen' xs n = filter p xs
  where
    bit = mode $ (transpose xs) !! n
    p x = (x !! n) == bit 

-- solution - how do we make this into a recursive function? 
solution = (oxygen 4) . (oxygen 3) . (oxygen 2) . (oxygen 1) . (oxygen 0) $ sample
solution' = oxygen 4 (oxygen 3 ( oxygen 2 (oxygen 1 (oxygen 0 sample))))
solution'' = oxygen' (oxygen' (oxygen' (oxygen' (oxygen' sample 0) 1) 2) 3) 4
solution''' = foldr oxygen sample [4,3,2,1,0]
solution'''' = foldl' oxygen' sample [0,1,2,3,4]

oxygenRating :: [String] -> [String]
oxygenRating xs = foldl' oxygen' xs [0..n-1]
  where
    n = length . head $ xs

oxygenFinal :: [String] -> [String]
oxygenFinal xs = foldl' go xs [0..n-1]
  where
    n = length . head $ xs
    go :: [String] -> Int -> [String]
    go [x] _ = [x]
    go xs n = filter p xs where
      bit = mode $ (transpose xs) !! n
      p x = (x !! n) == bit

-- co2

main :: IO ()
main = do
  inputs <- lines <$> readFile "inputs.txt"
  let gammaRate = gamma inputs
  let epsilonRate = epsilon inputs
  putStrLn $ "Gamma rate is " ++ show gammaRate
  putStrLn $ "Epsilon rate is " ++ show epsilonRate
  return ()
