module Main where

import Data.List

main :: IO ()
main = do
  s <- readFile "day1.txt"
  let v = splitLines s
  let f = filterLines v
  let parsed = parseLines f
  let sorted = sortLines parsed
  let dist = totalDist sorted
  print sorted
  print dist

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) = x : y : merge xs ys

concatList :: [a] -> [a] -> [a]
concatList xs [] = xs
concatList [] ys = ys
concatList (x : xs) ys = x : concatList xs ys

splitLine :: [Char] -> [[Char]]
splitLine v = case break (== ' ') v of
  (a, ' ' : rest) -> a : splitLine rest
  (a, "") -> [a, ""]
  (a, c) -> [a, c]

splitLines :: [Char] -> [[Char]]
splitLines "" = []
splitLines v = case break (== '\n') v of
  (a, '\n' : "") -> [a]
  (a, '\n' : rest) -> a : splitLines rest
  (a, "") -> [a]
  (a, c) -> [a, c]

filterLines :: [[Char]] -> [[[Char]]]
filterLines [] = []
filterLines [v] = [[head q, last $ init q]]
  where
    q = splitLine v
filterLines (v : rest) = filterLines [v] ++ filterLines rest

parseLines :: [[[Char]]] -> ([Integer], [Integer])
parseLines [] = ([], [])
parseLines [v] = ([read $ head v], [read $ head $ tail v])
parseLines (v : rest) = (merge (fst o) (fst o2), merge (snd o) (snd o2))
  where
    o = parseLines [v]
    o2 = parseLines rest

sortLines :: ([Integer], [Integer]) -> ([Integer], [Integer])
sortLines (v1, v2) = (sort v1, sort v2)

totalDist :: ([Integer], [Integer]) -> Integer
totalDist (_, []) = undefined
totalDist ([], _) = undefined
totalDist ([v1], [v2]) = abs v2 - v1
totalDist (v1 : rest1, v2 : rest2) = abs v2 - v1 + totalDist (rest1, rest2)
