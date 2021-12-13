module Main where

import Data.List.Split (splitOn)
import Data.List (group, sort, intersect, (\\))
import Control.Arrow ((&&&))

data Signal = Signal {
    patterns :: [String],
    outputs :: [String]
} deriving Show

data Display = Display {
    number :: Int,
    letters :: String
} deriving Show

-- given a list of lists, build a list of Signals
buildSignal :: [[String]] -> Signal
buildSignal [patterns, outputs] = Signal patterns outputs

parse :: String -> [Signal]
parse = map buildSignal . (map . map) words . map (splitOn " | ") . lines

displays :: [Display]
displays = 
    [ (Display 0 "abcefg")
    , (Display 1 "cf") 
    , (Display 2 "acdeg") 
    , (Display 3 "acdfg")
    , (Display 4 "bcdf")
    , (Display 5 "abdfg")
    , (Display 6 "abdefg")
    , (Display 7 "acf")
    , (Display 8 "abcdefg")
    , (Display 9 "abcdfg") ]

-- given a list of strings, return the set of chars that occurs exactly n times
occurs :: Int -> [String] -> String
occurs n s = map fst . filter ((==n) . snd). map (head &&& length) . group . sort . concat $ s

-- key maps to value (key, value)
mapWires :: [String] -> [(Char, Char)]
mapWires signals = (a, 'a'):(b, 'b'):(c, 'c'):(d, 'd'):(e, 'e'):(f, 'f'):(g, 'g'):[]
    where
        a = head $ (occurs 8 signals) \\ [c]
        b = head . occurs 6 $ signals
        c = head $ intersect (head . filter ((==2) . length) $ signals) (occurs 8 $ signals)
        d = head $ intersect (head . filter ((==4) . length) $ signals) (occurs 7 $ signals)
        e = head . occurs 4 $ signals
        f = head . occurs 9 $ signals
        g = head $ (occurs 7 signals) \\ [d] 

interpretSignal :: [Display] -> Signal -> Int
interpretSignal displays s = read . concat $ map (show . (decode displays)) ((map . map) (remap mapping) (outputs s))
    where
        mapping :: [(Char, Char)]
        mapping = mapWires (patterns s) 
        remap mapping c = snd . head . filter ((==c) . fst) $ mapping
        decode ds s = number . head . filter ((==(sort s)) . letters) $ ds

testCase = head . parse $ "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

solution1 :: [Signal] -> Int
solution1 = sum . map (length . filter (`elem` "1478") . show . interpretSignal displays)

solution2 :: [Signal] -> Int
solution2 = sum . map (interpretSignal displays)

main :: IO ()
main = do
    inputs <- parse <$> readFile "inputs.txt"
    print . solution1 $ inputs
    print . solution2 $ inputs
