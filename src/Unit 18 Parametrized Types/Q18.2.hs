-- Q18.2 Modify the Organ type so that it can be used as a key. 
-- Then build a Map, organInventory, of each organ to its count in the organCatalog.
import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- 1. Using functors
organInventory :: Map.Map Organ Int
organInventory = 
  let builder = Map.fromListWith (+) . (fmap . fmap $ (const 1))
  in builder $ zip organs ids 

-- 2. Using zipWith
organInventory' :: Map.Map Organ Int
organInventory' = Map.fromListWith (+) $ zipWith (\o _ -> (o, 1)) organs ids 

-- 3. just ignoring ids
organInventory'' :: Map.Map Organ Int
organInventory'' = Map.fromListWith (+) $ zip organs $ repeat 1