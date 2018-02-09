remove :: (a -> Bool) -> [a] -> [a]
remove f [] = []
remove f (x:xs) | f x = remove f xs
                | otherwise = x : remove f xs
                
myProduct :: Num a => [a] -> a
myProduct = foldl (*) 1