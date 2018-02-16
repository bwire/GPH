half :: Int -> Int
half = flip div 2 

printDouble :: Int -> String
printDouble = show . (*2)

makeAddressLambda :: Int -> String -> String -> (Int, String, String)
makeAddressLambda = 
  \number ->
  \street ->
  \town -> (number, street, town)
