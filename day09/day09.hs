module Main where

import Data.List (sort, reverse)
import Data.Maybe (fromJust)
import Data.Char (digitToInt)
import Data.Set (fromList, toList)

testCase = parse "2199943210\n3987894921\n9856789892\n8767896789\n9899965678" 

parse :: String -> [String]
parse = lines

findAdjacent :: Int -> Int -> [String] -> [Maybe Int]
findAdjacent x y mat = [Just p, left, right, top, bottom]
    where
        p = digitToInt $ mat !! x !! y
        xLength = length mat
        yLength = length . head $ mat
        left
          | y == 0 = Nothing
          | otherwise = Just $ digitToInt (mat !! x !! (y-1))
        right
          | y == (yLength - 1) = Nothing
          | otherwise = Just $ digitToInt (mat !! x !! (y+1))
        top
          | x == 0 = Nothing
          | otherwise = Just $ digitToInt (mat !! (x-1) !! y)
        bottom
          | x == (xLength - 1) = Nothing
          | otherwise = Just $ digitToInt (mat !! (x+1) !! y)
 
compare' :: Ord a => Maybe a -> Maybe a -> Ordering
compare' (Just a) Nothing = LT
compare' (Just a) (Just b) = a `compare` b
compare' _ _ = GT

(<<) :: Ord a => Maybe a -> Maybe a -> Bool
a << b = compare' a b == LT

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

comparisonMatrix :: [String] -> [[[Maybe Int]]]
comparisonMatrix mat = group (length . head $ mat) [findAdjacent x y mat | 
  let xMax = length mat - 1, let yMax = (length . head $ mat) - 1,
  x <- [0..xMax], y <- [0..yMax] ]

isLow :: [Maybe Int] -> Bool
isLow (p:ps) = all id $ map (p <<) ps

-- it is what it is man
compareLeft x y mat
    | left >= Just 9 = []
    | left <= p = []
    | otherwise = [(x,y-1)] ++ (compareLeft x (y-1) mat) ++ (compareUp x (y-1) mat) ++ (compareDown x (y-1) mat)
    where
        (p:left:right:top:bottom:[]) = mat !! x !! y

compareRight x y mat
    | right >= Just 9 = []
    | right <= p = []
    | otherwise = [(x,y+1)] ++ (compareRight x (y+1) mat) ++ (compareUp x (y+1) mat) ++ (compareDown x (y+1) mat)
    where
        (p:left:right:top:bottom:[]) = mat !! x !! y

compareUp x y mat
    | top >= Just 9 = []
    | top <= p = []
    | otherwise = [(x-1,y)] ++ (compareUp (x-1) y mat) ++ (compareLeft (x-1) y mat) ++ (compareRight (x-1) y mat)
    where
        (p:left:right:top:bottom:[]) = mat !! x !! y

compareDown x y mat
    | bottom >= Just 9 = []
    | bottom <= p = []
    | otherwise = [(x+1,y)] ++ (compareDown (x+1) y mat) ++ (compareLeft (x+1) y mat) ++ (compareRight (x+1) y mat)
    where
        (p:left:right:top:bottom:[]) = mat !! x !! y

nub' :: Ord a => [a] -> [a]
nub' = toList . fromList

basinScore x y mat
  | not $ isLow (mat !! x !! y) = 0
  | otherwise = 1 + (length . nub' $ (compareLeft x y mat ++ compareRight x y mat ++ compareUp x y mat ++ compareDown x y mat))

--basinScores :: [String] -> [[[Maybe Int]]]
basinScores mat = [basinScore x y mat | 
  let xMax = length mat - 1, let yMax = (length . head $ mat) - 1,
  x <- [0..xMax], y <- [0..yMax] ]

sumRisk :: [[Maybe Int]] -> Int
sumRisk xs = foldr f 0 xs
    where
        f b a = (fromJust . head $ b) + a + 1

 
-- solution1 :: [String] -> Int
solution1 s = sumRisk . concat . map (filter isLow) . comparisonMatrix $ s

solution2 s = product . take 3 . reverse . sort . basinScores . comparisonMatrix $ s

 
main :: IO ()
main = do
    inputs <- readFile "inputs.txt"
    print ""
    print . solution1 $ testCase
    print . solution1 . parse $ inputs
    print . solution2 . parse $ inputs
