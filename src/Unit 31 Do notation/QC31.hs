import Candidate

-- Quick check 31.1 Rewrite echo by using do-notation.
echo :: IO ()
echo = do 
  line <- getLine 
  putStrLn line

-- Quick check 31.2 Create a Candidate type and see whether that candidate is viable.
candidate = Candidate 175 A A MS

-- Quick check 31.3 Rewrite readGrade with do-notation.
readGrade :: IO Grade
readGrade = do
  grade <- getLine 
  return $ read grade

-- Quick check 31.4 Write a simple function Maybe String -> String that will print failed/passed 
-- if there’s a result and error id not found for the Nothing constructor.
mbStringToString :: Maybe String -> String
mbStringToString (Just v) = v
mbStringToString Nothing = "error id not found"  