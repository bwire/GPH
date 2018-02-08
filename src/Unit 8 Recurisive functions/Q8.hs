take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n - 1) xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

cycle' :: [a] -> [a]
cycle' xs = 
  let
    c' [] = c' xs
    c' (y:ys) = y : c' ys
  in c' xs

ack :: Int -> Int -> Int
ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) $ ack m (n - 1)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fib' 0 = 0
fib' 1 = 1
fib' 2 = 1
fib' n = f' 1 1 n where
  f' _ n2 2 = n2
  f' n1 n2 c = 
    let s = n1 + n2
    in f' n2 s (c - 1)
  
