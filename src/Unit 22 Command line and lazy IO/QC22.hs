-- Quick check 22.1 
-- Write a main that uses mapM to call getLine three times, and then use mapM_ to print out the values’ input. 
-- (Hint: You’ll need to throw away an argument when using mapM with getLine; use (\_ -> ...) to achieve this.)
main' :: IO ()
main' = do
  input <- mapM (\_ -> getLine) [(), (), ()]
  mapM_ putStrLn input


-- Quick check 22.2 Write your own version of replicateM, myReplicateM, that uses mapM. 
-- (Don’t worry too much about the type signature.)
myReplicateM :: (Monad m, Enum a, Num a) => a -> m b -> m [b]
myReplicateM n action = mapM (\_ -> action) [1..n]

--Quick check 22.3 Use lazy I/O to write a program that reverses your input and prints it back to you.
main'' :: IO ()
main'' = do
  input <- getContents
  putStrLn $ reverse input

-- Quick check 22.4 Write a program that returns the sum of the squares of the input.
main :: IO ()
main = do
  input <- getContents
  putStrLn $ "The sum is - " ++ (show . sum . map ((\x -> x * x) . read) . lines $ input)



