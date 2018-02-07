-- Q17.1 Your current implementation of Color doesn’t contain an identity element. 
-- Modify the code in this unit so that Color does have an identity element, and then make Color an instance of Monoid.
import Data.Semigroup

data Color = Red 
           | Yellow 
           | Blue 
           | Green 
           | Purple 
           | Orange 
           | Brown
           | Black deriving (Show, Eq)

instance Semigroup Color where
  Blue <> Yellow = Green
  Yellow <> Blue = Green
  Red <> Blue = Purple
  Blue <> Red = Purple
  Red <> Yellow = Orange
  Yellow <> Red = Orange
  a <> Black = a
  Black <> a = a
  a <> b | a == b = a
         | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
         | all (`elem` [Red, Blue, Purple]) [a, b] = Green
         | all (`elem` [Red, Yellow, Orange]) [a, b] = Green

instance Monoid Color where
  mempty = Black
  mappend = (<>)