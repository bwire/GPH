import Dist hiding (main)
import Data.Map as Map
import Data.Maybe (fromJust)

-- Q28.1 Writing haversineMaybe (listing 28.4) was straightforward. Write the function haversineIO without using <*>. 
-- Here’s the type signature:
-- haversineIO :: IO LatLong -> IO LatLong -> IO Double

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO iol1 iol2 = do
  ll1 <- iol1
  ll2 <- iol2
  return $ haversine ll1 ll2

-- Q28.2 Rewrite haversineIO, this time using <*>
haversineIO' :: IO LatLong -> IO LatLong -> IO Double
haversineIO' iol1 iol2 = haversine <$> iol1 <*> iol2

-- Q28.3 Recall the RobotPart type from the preceding lesson:

-- data RobotPart = RobotPart
--    { name :: String
--    , description :: String
--    , cost :: Double
--    , count :: Int
--    } deriving Show

-- Make a command-line application that has a database of various RobotParts (at least five), 
-- and then lets the user enter in two-part IDs and returns the one with the lowest cost. 
-- Handle the case of the user entering an ID that’s not in the parts database.

-- data RobotPart = RobotPart
--    { name :: String
--    , description :: String
--    , cost :: Double
--    , count :: Int
--    } deriving Show

data RobotPart = RobotPart
  { name :: String
  , description :: String
  , cost :: Double
  , count :: Int
  } deriving Show

leftArm :: RobotPart
leftArm  = RobotPart
   { name = "left arm"
   , description = "left arm for face punching!"
   , cost = 1000.00
   , count = 3
   }
 
rightArm :: RobotPart
rightArm  = RobotPart
   { name = "right arm"
   , description = "right arm for kind hand gestures"
   , cost = 1025.00
   , count = 5
   }
 
robotHead :: RobotPart
robotHead  = RobotPart
   { name = "robot head"
   , description = "this head looks mad"
   , cost = 5092.25
   , count = 2
   }

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList . zip [1, 2, 3] $ [leftArm, rightArm, robotHead]

main :: IO ()
main = do
  putStrLn "Enter first part id"
  firstID <- read <$> getLine
  putStrLn "Enter second part id"
  secondID <- read <$> getLine
  let firstPart = Map.lookup firstID $ partsDB
  let secondPart = Map.lookup secondID partsDB
  let f p1 p2 = (<) <$> (cost <$> p1) <*> (cost <$> p2) 
  case f firstPart secondPart of
    Just True -> putStrLn . show . fromJust $ firstPart
    Just False -> putStrLn . show . fromJust $ secondPart
    Nothing -> putStrLn "wrong id provided"

