myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

myGCD a b = mgcd' (a `mod` b) b where
  mgcd' 0 b = b
  mgcd' rem b = myGCD b rem 