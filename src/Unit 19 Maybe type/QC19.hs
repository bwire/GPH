import qualified Data.Map as Map

-- Quick check 19.2 Write a function numOrZero that takes a Maybe Int and returns 0 if it’s nothing, and otherwise returns the values.

numOrZero :: Maybe Int -> Int
numOrZero (Just v) = v
numOrZero _ = 0

-- Quick check 19.3 How would you rewrite report so that it works with Maybe (Location,Container) and handles the case of the missing Organ?
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
   show (Vat organ) = show organ ++ " in a vat"
   show (Cooler organ) = show organ ++ " in a cooler"
   show (Bag organ) = show organ ++ " in a bag"
   
data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat o) = (Lab, Vat o)
placeInLocation (Cooler o) = (Lab, Cooler o)
placeInLocation (Bag o) = (Kitchen, Bag o)

processAndReport :: Maybe Organ -> String
processAndReport o = report . process $ o

process :: Maybe Organ -> Maybe (Location, Container)
process (Just o) = Just . placeInLocation . organToContainer $ o
process _ = Nothing

report :: Maybe (Location,Container) -> String
report (Just (location, container)) = show container ++ " in the " ++ show location
report Nothing = "error, id not found"
  
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
 where organ = Map.lookup id catalog