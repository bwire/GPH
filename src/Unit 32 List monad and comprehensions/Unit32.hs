powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  v <- [1 .. n]
  return (2 ^ v, 3 ^ v)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  ev <- [2, 4 .. n]
  ov <- [1, 3 .. n]
  return (ev, ov)
