--Q18.1 For the types Triple and Box, implement a function similar to map, tripleMap, and boxMap.

data Triple a = Triple a a a deriving Show

first :: Triple a -> a
first (Triple v _ _) = v

second :: Triple a -> a
second (Triple _ v _) = v

third :: Triple a -> a
third (Triple _ _ v) = v

toList :: Triple a -> [a]
toList (Triple f s t) = [f, s, t]

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

transform :: (a -> a) -> Triple a -> Triple a
transform = tripleMap

data Box a = Box a deriving Show

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box $ f x

