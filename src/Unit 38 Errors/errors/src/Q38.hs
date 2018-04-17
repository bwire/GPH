module Q38 where

import Data.Char (isDigit)

-- Q38.1

-- Make a function addStrInts that takes two Ints represented as Strings and adds them. 
-- The function would return an Either String Int. 
-- The Right constructor should return the result, provided that the two arguments can be parsed into Ints 
-- (use Data.Char isDigit to check). Return a different Left result for the three possible cases:

-- First value can’t be parsed.
-- Second value can’t be parsed.
-- Neither value can be parsed.

addStrInt :: String -> String -> Either String Int
addStrInt s1 s2 = 
  let check = any (== False) . map isDigit
  in f' (check s1) (check s2) where
    f' res1 res2  | res1 && res2 = Left "Neither value can be parsed"
                  | res1 = Left "First value cannot be parsed"
                  | res2 = Left "Second value cannot be parsed" 
                  | otherwise = Right $ read s1 + read s2 

-- Q38.2

-- The following are all partial functions. Use the type specified to implement a safer version of the function:

-- succ—Maybe
-- tail—[a] (Keep the type the same.)
-- last—Either (last fails on empty lists and infinite lists; use an upper bound for the infinite case.)

safeSucc :: (Enum a, Ord a, Bounded a) => a -> Maybe a
safeSucc n = if n > maxBound
          then Nothing
          else Just $ succ n

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

safeLast :: Show a => [a] -> Either String a
safeLast [] = Left "The list is empty"
safeLast xs = sl' 1 xs where
  sl' :: Int -> [a] -> Either String a
  sl' n ys | n > maxBound = Left "The list is infinite"
           | null (safeTail ys) = Right (head ys)
           | otherwise = sl' (n + 1) (safeTail ys) 
  