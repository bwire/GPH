-- Q6.1
repeat' :: a -> [a]
repeat' v = v : repeat' v 

-- Q6.2
subseq :: Int -> Int -> [a] -> [a]
subseq start end = drop start . take end 

-- Q6.3
inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf e xs = e `elem` (take (length xs `div` 2) xs)