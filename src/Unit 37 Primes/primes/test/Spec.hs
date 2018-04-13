import Test.QuickCheck
import Data.Maybe (isJust)
import Primes

prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val = 
  if val < 2 || val >= length primes
  then result == Nothing
  else isJust result where
    result = isPrime val

prop_primesArePrime :: Int -> Bool
prop_primesArePrime val = 
  if result == Just True
  then length divisors == 0
  else True where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite :: Int -> Bool
prop_nonPrimesAreComposite val = 
  if result == Just False
  then length divisors > 0
  else True where 
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val = 
  if result == Nothing
  then True
  else Just val == (foldr (*) 1 <$> result) 
    where result = primeFactors val 

prop_allFactorsPrime :: Int -> Bool
prop_allFactorsPrime val = 
  if result == Nothing
  then True
  else Just True == (all (isJust . isPrime) <$> result)
    where result = primeFactors val

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
