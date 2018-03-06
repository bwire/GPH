-- Quick check 30.4 Turn (+ 2) from type Num a => a -> a to type Num a => a -> IO a using a lambda and return. 
-- Use :t in GHCi to double-check that you’re getting the correct type.

plusTwoIO :: Num a => a -> IO a
plusTwoIO = return . (+2)