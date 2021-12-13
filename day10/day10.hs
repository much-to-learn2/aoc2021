
module Main where

import Data.List (sort)

data Record = Record {
    char :: Char,
    idx :: Int,
    match :: Maybe Int
}

testCase = parse $ "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

parse = lines

pair '[' = ']'
pair '{' = '}'
pair '(' = ')'
pair '<' = '>'
pair ']' = '['
pair '}' = '{'
pair ')' = '('
pair '>' = '<'
pair  _  = ' '


insert x [] = [x]
insert x@(char1, idx1, pair1) y@((char2, idx2, pair2):rest) = if idx1 <= idx2 then x:y
                                                              else (char2, idx2, pair2) : insert x rest

-- checkSyntax [(Opening character, index)] [rest of string being parsed] []
checkSyntax s = go [] [] 0 s
  where
    go [] r n (x:xs) = go [(x, 0)] r 1 xs
    go (o:os) r n (x:xs)
      -- new opening pair
      | x `elem` "[{(<" = go ((x, n) : o:os ) r (n+1) xs
      -- matched pair
      | pair x == fst o = go os (insert (x, n, Just (snd o)) (insert (fst o, snd o, Just n) r)) (n+1) xs
      -- incorrectly matched pair - return the two that didn't match
      | pair x /= fst o = [(fst o, snd o, Just n) , (x, n, Just (snd o))]
    -- once we run out of things to parse, add the rest of the openings to the result with no match
    go o r n [] = foldr f r o
      where
        f (char, idx) a = insert (char, idx, Nothing) a

isCorrupt [(char1, idx1, match1), (char2, idx2, match2)] = char1 /= pair char2
isCorrupt _ = False

firstCorrupt x 
  | isCorrupt parsed = idx . head . tail $ parsed
  | otherwise = ' '
  where
      parsed = checkSyntax x
      idx (c, i, p) = c

incomplete [] = False
incomplete (x:xs)
  | missingPair x = True
  | otherwise = incomplete xs
  where
      missingPair (char, idx, Nothing) = True
      missingPair _ = False 

autocomplete = map (missingPair) . filter p . checkSyntax
  where
    p (c, i, Nothing) = True
    p _ = False
    missingPair (c, i, p) = pair c

autocompleteScore x = foldr (\b a -> 5*a + b) 0 charScores
  where
      charScores = map syntaxScore' (autocomplete x)

median xs
  | odd len = sorted !! ((len-1) `div` 2)
  | even len = (sorted !! (len `div` 2 -1) + sorted !! (len `div` 2) ) `div` 2
  where
      len = length xs
      sorted = sort xs

test = sum . map (syntaxScore . firstCorrupt) $ testCase

syntaxScore ')' = 3
syntaxScore ']' = 57
syntaxScore '}' = 1197
syntaxScore '>' = 25137
syntaxScore _ = 0

syntaxScore' ')' = 1
syntaxScore' ']' = 2
syntaxScore' '}' = 3
syntaxScore' '>' = 4
syntaxScore' _ = 0

solution1 = sum . map (syntaxScore . firstCorrupt)
solution2 = median . filter (>0) . map autocompleteScore

main :: IO ()
main = do
    inputs <- readFile "inputs.txt"
    print "hello"
    print . solution1 . parse $ inputs
    print . solution2 . parse $ inputs
