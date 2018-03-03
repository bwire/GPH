-- Quick check 29.1 Use <$> and <*> to combine two Maybe String types with ++.
concatMB :: Maybe String -> Maybe String -> Maybe String
concatMB mbs1 mbs2 = (++) <$> mbs1 <*> mbs2


-- Quick check 29.2 Make the String "Hello World" into an IO String.
stringToIO :: String -> IO String
stringToIO = pure

-- Quick check 29.4 Solve this problem if the boxes contain a prize multiplier instead of just an additional prize. 
-- The multipliers are 10× and 50×.
doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int -> Int]
boxPrize = [(*10), (*50)]

totalPrize :: [Int]
totalPrize = boxPrize <*> doorPrize

