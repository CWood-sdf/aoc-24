module Main where

main :: IO ()
main = do
  s <- readFile "day2.txt"
  let v = splitLines s
  let f = filterLines v
  let parsed = parseLines f
  let list = [x | x <- parsed, lineGoodErrors x]
  print list
  print $ length list

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
filterLines [v] = [[x | x <- q, x /= " " && x /= ""]]
  where
    q = splitLine v
filterLines (v : rest) = filterLines [v] ++ filterLines rest

parseLines :: [[[Char]]] -> [[Int]]
parseLines [] = []
parseLines [v] = [[read q :: Int | q <- v]]
parseLines (v : rest) = o ++ o2
  where
    o = parseLines [v]
    o2 = parseLines rest

isGood :: Int -> Int -> Bool -> Bool
isGood v1 v2 desc = (v1 < v2) == desc && abs (v2 - v1) <= 3 && abs (v2 - v1) >= 1

lineGoodSub :: [Int] -> Bool -> Bool
lineGoodSub [] _ = True
lineGoodSub [_] _ = True
lineGoodSub (v : rest) q =
  isGood v (head rest) q && lineGoodSub rest q

lineGood :: [Int] -> Bool
lineGood [] = True
lineGood v = lineGoodSub v (head v < head (tail v))

b2i :: Bool -> Int
b2i True = 1
b2i False = 0

removeIndex :: [a] -> Int -> [a]
removeIndex [] _ = []
removeIndex (i : list) v | v > 0 = i : removeIndex list (v - 1)
removeIndex (_ : rest) v | v == 0 = rest
removeIndex (i : list) _ = i : list

goodWithRemoval :: [Int] -> Int -> Bool
goodWithRemoval list i | i >= length list = False
goodWithRemoval list i = lineGood (removeIndex list i) || goodWithRemoval list (i + 1)

lineGoodErrors :: [Int] -> Bool
lineGoodErrors list = lineGood list || goodWithRemoval list 0
