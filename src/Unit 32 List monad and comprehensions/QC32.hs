import Control.Applicative
import Data.Char (toUpper)

-- Quick check 32.1 Use do-notation to generate pairs of numbers up to 10 and their squares.
squarePairs :: [(Int, Int)]
squarePairs = do
  n <- [1 .. 10]
  return (n, n * n)

-- Quick check 32.2 Write filter by using guard and do-notation.
guard :: Alternative f => Bool -> f ()
guard p = if p 
          then pure ()
          else empty 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f list = do
   value <- list
   guard(f value)
   return value

-- Quick check 32.3 Write a list comprehension that takes the following words
-- ["brown","blue","pink","orange"]
-- and capitalizes the first letter, and prepends Mr. in front. 
trans :: [String] -> [String]
trans lst = [mconcat ["Mr. ", (toUpper . head) v : tail v] | v <- lst]