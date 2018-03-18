-- Q32.1 Use a list comprehension that generates a list of correct calendar dates, 
-- given that you know the number of days in each month. 
-- For example, it should start with 1 .. 31 for January and be followed by 1 .. 28 for February.
datesGenerator :: [Int]
datesGenerator = [d | m <- [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], d <- [1 .. m]]

-- Q32.2 Translate the preceding question into do-notation, and then into Monad methods and lambdas.
datesGeneratorDo :: [Int]
datesGeneratorDo = do
  m <- [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  d <- [1 .. m]
  return d

datesGeneratorLambda :: [Int]
datesGeneratorLambda = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] >>= \m -> [1..m]
