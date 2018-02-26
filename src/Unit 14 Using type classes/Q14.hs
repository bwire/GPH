import System.Random

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum) 

instance Show SixSidedDie where
   show S1 = "one"
   show S2 = "two"
   show S3 = "three"
   show S4 = "four"
   show S5 = "five"
   show S6 = "six"

-- Q14.1 Note that Enum doesn’t require either Ord or Eq, 
-- even though it maps types to Int values (which implement both Ord and Eq). 
-- Ignoring the fact that you can easily use deriving for Eq and Ord, 
-- use the derived implementation of Enum to make manually defining Eq and Ord much easier.
instance Eq SixSidedDie where
  (==) dl dr = fromEnum dl == fromEnum dr

instance Ord SixSidedDie where
  compare dl dr = fromEnum dl `compare` fromEnum dr


-- Q14.2 Define a five-sided die (FiveSidedDie type). 
-- Then define a type class named Die and at least one method that would be useful to have for a die. 
-- Also include superclasses you think make sense for a die. Finally, make your FiveSidedDie an instance of Die.
data FiveSidedDie = FSD1 | FSD2 | FSD3 | FSD4 | FSD5 | FSD6 deriving (Enum)

instance Show FiveSidedDie where
   show FSD1 = "one"
   show FSD2 = "two"
   show FSD3 = "three"
   show FSD4 = "four"
   show FSD5 = "five"

instance Eq FiveSidedDie where
  (==) dl dr = fromEnum dl == fromEnum dr

instance Ord FiveSidedDie where
  compare dl dr = fromEnum dl `compare` fromEnum dr 

class Die a where
  throw :: IO (a, a)

instance Die FiveSidedDie where
  throw = do
    id1 <- randomRIO (0, 4)
    id2 <- randomRIO (0, 4)
    return (toEnum id1, toEnum id2)

showDice :: (FiveSidedDie, FiveSidedDie) -> IO ()
showDice (fd, sd) = putStrLn $ "(" ++ show fd ++ ", " ++ show sd ++ ")" 



