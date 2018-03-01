import Dist hiding (main)
import Min hiding (main)

-- Quick check 28.1 Write addMaybe for adding two Maybe Ints.

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe mbi1 mbi2 = (+) <$> mbi1 <*> mbi2

-- Quick check 28.2 Suppose you don’t have to worry about Maybes and have raw coordinate pairs. 
-- If you have the pair newYork, how would you make a function distanceFromNY that’s waiting for an additional location?
newYork :: LatLong
newYork = (1, 1)

distanceFromNY :: LatLong -> Double
distanceFromNY = haversine newYork

-- Quick check 28.3 Use the pattern for using binary values in a context for the functions (*), div, and mod on these two values:
val1 = Just 10
val2 = Just 5

val3 = (*) <$> val1 <*> val2
val4 = div <$> val1 <*> val2
val5 = mod <$> val1 <*> val2

-- Quick check 28.4 Use minOfThree to get the Maybe Int value of these three Maybe values:
-- Just 10
-- Just 3
-- Just 6
minOfMaybeInts :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
minOfMaybeInts v1 v2 v3 = minOfThree <$> v1 <*> v2 <*> v3  