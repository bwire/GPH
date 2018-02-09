import Data.Char (toLower, isSpace)

-- Q9.1 Use filter and length to re-create the elem function.
elem' :: Eq a => a -> [a] -> Bool
elem' e = (/= 0) . length . filter (==e)

-- Q9.2 Your isPalindrome function from lesson 6 doesn’t handle sentences with spaces or capitals. 
-- Use map and filter to make sure the phrase “A man a plan a canal Panama” is recognized as a palindrome.
isPalindrome :: String -> Bool
isPalindrome xs = 
  let transformed = map toLower . filter isSpace $ xs
  in reverse transformed == transformed
  
-- Q9.3 In mathematics, the harmonic series is the sum of 1/1 + 1/2 + 1/3 + 1/4 .... 
-- Write a function harmonic that takes an argument n and calculates the sum of the series to n. Make sure to use lazy evaluation.
harmonic :: Int -> Double
harmonic n = foldr (\e a -> a + 1 / fromIntegral e) 0 [1..n]
  
  