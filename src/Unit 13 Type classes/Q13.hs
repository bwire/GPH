cycleSucc :: (Bounded a, Enum a, Ord a) => a -> a
cycleSucc n = if n < maxBound
              then succ n
              else minBound