module Main where 

data Delta = Increase | Decrease | None
  deriving (Show)

parseInput :: String -> [Int]
parseInput = map read . words

quantifyDelta :: [Int] -> [Delta]
quantifyDelta [] = []
quantifyDelta [x] = []
quantifyDelta (x:y:ys)
  | x < y = Increase : quantifyDelta (y:ys)
  | x == y = None : quantifyDelta (y:ys)
  | x > y = Decrease : quantifyDelta (y:ys)

countIncreases :: [Delta] -> Int
countIncreases = foldr increased 0 
  where
    increased :: Delta -> Int -> Int
    increased Increase acc = acc + 1
    increased _ acc = acc

rollingSum :: Int -> [Int] -> [Int]
rollingSum _ [] = []
rollingSum n list@(x:xs) 
  | length list < n = []
  | otherwise = sum (take n list)  : rollingSum n xs

main :: IO ()
main = do
  inputs <- readFile "inputs.txt"
  let numIncrease = countIncreases . quantifyDelta . parseInput $ inputs
  putStrLn $ "There are " ++ show numIncrease ++ " measurements that are larger than the previous measurement."

  let numIncreaseRolling = countIncreases . quantifyDelta . rollingSum 3 . parseInput $ inputs
  putStrLn $ "There are " ++ show numIncreaseRolling 
    ++ " rolling measurements that are larger than the previous measurement."
  return ()
