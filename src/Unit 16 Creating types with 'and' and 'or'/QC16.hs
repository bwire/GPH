-- Quick check 16.1 Rewrite AuthorName by using record syntax.
data AuthorName = AuthorName 
  { firstName :: String
  , lastName :: String }

-- Quick check 16.2 Assume you have a Car type. 
-- How could you represent a SportsCar as a Car with a Spoiler? (Assume that you have a Spoiler type as well.)
data Car = Car
data Spoiler = Spoiler
data SportsCar = SportsCar Car Spoiler 