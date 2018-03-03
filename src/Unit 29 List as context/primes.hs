import Control.Applicative (liftA2)

primesToN :: Int -> [Int]
primesToN n = 
  let toN = [2..n]
      composite = liftA2 (*) toN toN
      filterF = not . (`elem` composite)
  in filter filterF toN