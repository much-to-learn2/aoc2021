{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Char (isUpper)
import Data.List (nub, (\\), sort, group)

testCase10 = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
testCase19 = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"
testCase226 = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"

-- caves can either be big or small, Integer represents how many times they have been visited
data Cave = Big String | Small String deriving (Show, Eq, Ord)

-- instance Eq Cave where
--     (Big s) == (Big s') = s == s'
--     (Small s) == (Small s') = s == s'
--     _ == _ = False

start = (Small "start")
end = (Small "end")

data Path = Path Cave Cave deriving (Show, Eq)

type State = [Cave]

-- given a String, transform it into a big or small Cave
toCave :: String -> Cave
toCave s
  | s == "start" = start
  | all isUpper s = Big s
  | otherwise = Small s

-- given a string of form "c1-c2", return a path
toPath :: String -> Path
toPath s = Path cave1 cave2
  where
      cave1 = toCave . takeWhile (/= '-') $ s
      cave2 = toCave . tail . dropWhile (/= '-') $ s

parse :: String -> [Path]
parse = map toPath . lines

-- given a current position and a path, travel through to the new position
spelunk :: Cave -> Path -> Cave
spelunk c (Path p1 p2)
  | c == p1 = p2
  | c == p2 = p1
  | otherwise = c

-- given a current position and a path, return if that path is valid for current position
isValid :: Cave -> Path -> Bool
isValid c (Path p1 p2)
  | c /= p1 && c /= p2 = False
  | otherwise = True

-- initial state
state0 :: State
state0 = start:[]

-- given a state and a path, update the state to the new state after path has been followed
incrementState :: State -> Path -> State
incrementState s p 
  | head s == end = s
  | otherwise = spelunk (head s) p : s

-- given a [Cave] and a path, determine if that path touches any of the caves
-- this took SO LONG
pathTouchesCaves :: Path -> [Cave] -> Bool
pathTouchesCaves = any . flip isValid

-- given a state and list of paths, determine which have collapsed
-- NOTE: Ignores the head of the caves (current position) so small caves only collapse when you leave
collapsedPaths :: State -> [Path] -> [Path]
collapsedPaths s@(cave:caves) = filter (`pathTouchesCaves` collapsedCaves)
  where
      collapsedCaves = nub . filter isSmall $ caves
      isSmall (Small c) = True
      isSmall _ = False

-- for this one, we are allowed to travel to one small cave twice
collapsedPaths' :: State -> [Path] -> [Path]
collapsedPaths' !s@(cave:caves) !ps
  | firstStep = []
  | not travelledTwice = filter (`pathTouchesCaves` [start]) ps
  | otherwise = filter (`pathTouchesCaves` collapsedCaves) ps
  where
      firstStep = length caves < 2
      travelledTwice = any (>=2) . map length . group . sort . filter isSmall $ caves
      collapsedCaves = nub . filter isSmall $ caves
      isSmall (Small c) = True
      isSmall _ = False

-- given a State and list of Paths, return a list of states from all possible moves
possibleStates :: [Path] -> State -> [State]
possibleStates !ps !s@(cave:caves) = map (incrementState s) validPaths
  where
      validPaths = (filter (isValid cave) ps) \\ collapsedPaths' s ps

-- given list of paths and a list of states, step to the next possible list of states
allPossibleStates :: [Path] -> [State] -> [State]
allPossibleStates !ps = nub . concat . map (possibleStates ps) 

collapseStates :: [State] -> (Int, [State])
collapseStates !s = (n, s')
  where
    s' = filter ((/=)end . head) s
    n = length . filter ((==)end . head) $ s

stepToCompletion :: [Path] -> [State] -> [State]
stepToCompletion !ps !s
  | allTerminated = s
  | otherwise = stepToCompletion ps s'
  where
      !s' = allPossibleStates ps s
      !heads = map head s
      !allTerminated = all (==end) heads

stepToCompletion' :: [Path] -> (Int, [State]) -> (Int, [State])
stepToCompletion' !ps !(!n, !s)
  | allTerminated = (n, s)
  | otherwise = stepToCompletion' ps (n + n', s')
  where
      !(!n', !s') = collapseStates $ allPossibleStates ps s
      allTerminated = null s


solution :: [Path] -> Int
solution !p = fst $ stepToCompletion' p (0, [state0])

main :: IO ()
main = do
    inputs <- parse <$> readFile "inputs.txt"
    return ()
    print . solution . parse $ testCase10
    print . solution . parse $ testCase19
    print . solution . parse $ testCase226    
    print . solution $ inputs