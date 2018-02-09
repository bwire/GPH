-- Q19.1 Write a function emptyDrawers that takes the output of getDrawerContents and tells you the number of drawers that are empty.
import qualified Data.Map as Map
import Data.Maybe

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
 where getContents = \id -> Map.lookup id catalog
 
emptyDrawers :: [Int] -> Map.Map Int Organ -> Int
emptyDrawers ids = length . filter isNothing  . getDrawerContents ids

-- Q19.2 Write a version of map that works for Maybe types, called maybeMap
maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap f (x:xs) = fmap f x : maybeMap f xs
  
