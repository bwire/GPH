import qualified Data.Map as Map
import Data.List
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]
 
file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]
 
file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]
 
file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]
        
data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = 
  let completeTimes = [minimum times..maximum times]
      timeValueMap = Map.fromList $ zip times values
      extendedValue = map (\t -> Map.lookup t timeValueMap) completeTimes
  in TS completeTimes extendedValue

fileToTS :: [(Int, a)] -> TS a
fileToTS = uncurry createTS . unzip

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just v) = mconcat [show time, "|", show v, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat $ zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair map (_, Nothing) = map
insertMaybePair map (k, Just v) = Map.insert k v map

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1 
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where bothTimes = mconcat [t1, t2]
        completeTimes = [minimum bothTimes..maximum bothTimes]
        tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
        combinedValues = map (\t -> Map.lookup t updatedMap) completeTimes 

instance Semigroup (TS a) where
  (<>) = combineTS  

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>) 

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: Real a => [a] -> Double
mean xs = 
  let total = realToFrac . sum $ xs
      count = realToFrac . length $ xs
  in total / count
  
meansTS :: Real a => TS a -> Maybe Double
meansTS (TS [] []) = Nothing
meansTS (TS t v) =
  if all (== Nothing) v
  then Nothing
  else Just avg 
    where
      justVals = filter isJust v
      cleanVals = map fromJust justVals
      avg = mean cleanVals

-- comparison functions
type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newfunc where
  newfunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
  newfunc p (_, Nothing) = p
  newfunc (_, Nothing) p = p
  newfunc (i1, Just v1) (i2, Just v2) = if func v1 v2 == v1
                                        then (i1, Just v1)  
                                        else (i2, Just v2) 

compareTS :: Eq a => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS func (TS times values) = 
  if all (== Nothing) values
  then Nothing
  else Just best where  
    pairs = zip times values
    best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just v1) (Just v2) = Just (v1 - v2)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times(Nothing : diffValues)
  where shiftValues = tail values
        diffValues = zipWith diffPair shiftValues values   

-- averaging
meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe [] = Nothing
meanMaybe vals = 
  if any (== Nothing) vals
  then Nothing
  else (Just avg) where
    avg = mean $ map fromJust vals

movingAvg :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingAvg vals chunk = 
  if length vals >= chunk 
  then (meanMaybe $ take chunk vals) : movingAvg (tail vals) chunk
  else [] 

movingAverageTS :: Real a => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times smothedValues 
  where ma = movingAvg values n
        nothings = replicate (n `div` 2) Nothing
        smothedValues = mconcat [nothings, ma, nothings]
