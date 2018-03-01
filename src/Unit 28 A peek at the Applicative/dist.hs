module Dist where

import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList 
  [("Arkham",(42.6054,-70.7829))
  ,("Innsmouth",(42.8250,-70.8150))
  ,("Carcosa",(29.9714,-90.7694))
  ,("New York",(40.7776,-73.9691))]

toRadians :: Double -> Double
toRadians = (*) (pi / 180)
 
latLongToRads :: LatLong -> (Double, Double)
latLongToRads = (,) <$> toRadians . fst <*> toRadians . snd
 
haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c        
 where (rlat1, rlong1) = latLongToRads coords1
       (rlat2, rlong2) = latLongToRads coords2
       dlat = rlat2 - rlat1
       dlong = rlong2 - rlong1      
       a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^ 2
       c = 2 * atan2 (sqrt a) (sqrt (1 - a))
       earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

main :: IO ()
main = do
  putStrLn "Enter the first city name"
  startPoint <- getLine
  putStrLn "Enter the second city name"
  endPoint <- getLine
  printDistance $
    haversine <$> Map.lookup startPoint locationDB <*> Map.lookup endPoint locationDB

