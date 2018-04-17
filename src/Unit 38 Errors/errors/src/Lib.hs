module Lib where

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n - 1) xs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> [a] -> Maybe [a]
myTakeSafer _ [] = Nothing
myTakeSafer n (x:xs) = (:) <$> Just x <*> myTakeSafer (n - 1) xs

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:_) = Right x

eitherTail :: [a] -> Either String [a]
eitherTail [] = Left "There is no tail because the list is empty"
eitherTail (_:xs) = Right xs

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

-- QUICK CHECK 38.4
addFirstAndSecond :: [Int] -> Either String Int
addFirstAndSecond xs = (+) <$> eitherHead xs <*> (eitherTail xs >>= eitherHead) 

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
   show TooLarge = "Value exceed max bound"
   show InvalidValue = "Value is not a valid candidate for prime checking"

maxN :: Int
maxN = 10

primes :: [Int]
primes = sieve [2..10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` x)) xs

isPrime :: Int -> Either PrimeError Bool
isPrime n
   | n < 2 = Left InvalidValue
   | n > maxN = Left TooLarge
   | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

