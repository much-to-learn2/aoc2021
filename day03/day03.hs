module Main where

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
oxygen :: [String] -> Int
oxygen xs = bin2dec . read . head $ (foldl' go xs [0..n-1])
  where
    n = length . head $ xs
    go :: [String] -> Int -> [String]
    go [x] _ = [x]
    go xs n = filter p xs where
      bit = mode $ (transpose xs) !! n
      p x = (x !! n) == bit

-- co2
co2 :: [String] -> Int
co2 xs = bin2dec . read . head $ (foldl' go xs [0..n-1])
  where
    n = length . head $ xs
    go :: [String] -> Int -> [String]
    go [x] _ = [x]
    go xs n = filter p xs where
      bit = antimode $ (transpose xs) !! n
      p x = (x !! n) == bit

main :: IO ()
main = do
  inputs <- lines <$> readFile "inputs.txt"
  let gammaRate = gamma inputs
  let epsilonRate = epsilon inputs
  putStrLn $ "Gamma rate is " ++ show gammaRate
  putStrLn $ "Epsilon rate is " ++ show epsilonRate
  putStrLn $ "Total Power consumption is " ++ show (gammaRate * epsilonRate)

  let oxygenRate = oxygen inputs
  let co2Rate = co2 inputs
  putStrLn $ "Oxygen rate is " ++ show oxygenRate
  putStrLn $ "CO2 rate is " ++ show co2Rate
  putStrLn $ "Life support rating is " ++ show (oxygenRate * co2Rate)
  return ()
