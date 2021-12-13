module Main  where

import Data.List

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case 
    dropWhile p s of
        "" -> []
        s' -> w : splitOn p s''
            where 
                (w, s'') = break p s'

count :: Integer -> [Integer] -> Integer
count n = fromIntegral . length . filter (==n)

-- so how does this work? 
count' :: Int -> [Int] -> Int
count' = (length .) . filter . (==)

parse :: String -> [Integer]
parse = map read . splitOn (==',')

fishCount :: [Integer] -> [Integer]
fishCount fish = map ($ fish) $ map count [0..8] 

stepFish :: [Integer] -> [Integer]
stepFish [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]

day06 :: Int -> [Integer] -> Integer
day06 n xs = sum $ iterate stepFish xs !! n

main :: IO ()
main = do
    inputs <- readFile "inputs.txt"
    print . day06 80 . fishCount . parse $ inputs
    print . day06 256 . fishCount . parse $ inputs
