module Primes where

primes :: [Int]
primes = sieve [2..10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` x)) xs

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n > length primes = Nothing
          | otherwise = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 _ = []
unsafePrimeFactors _ [] = []
unsafePrimeFactors n xx@(x : xs) = 
  if n `mod` x == 0 
  then x : unsafePrimeFactors (n `div` x) xx
  else unsafePrimeFactors n xs

primeFactors :: Int -> Maybe [Int]
primeFactors n 
  | n < 2 = Nothing
  | n > length primes = Nothing
  | otherwise = Just $ unsafePrimeFactors n primesLessThanN 
    where primesLessThanN = filter (<= n) primes