-- Q27.1 When we introduced parameterized types in lesson 15, you used a minimal type Box as the example:
-- data Box a = Box a deriving Show
-- Implement the Functor type class for Box. Then implement morePresents, 
-- which changes a box from type Box a to one of type Box [a], 
-- which has n copies of the original value in the box in a list. Make sure to use fmap to implement this.

data Box a = Box a deriving Show 

instance Functor Box where
  fmap f (Box v) = Box . f $ v

morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap (take n . repeat) 

-- QC27.2 Now suppose you have a simple box like this: 
-- myBox :: Box Int
-- myBox = Box 1

-- Use fmap to put the value in your Box in another Box. 
-- Then define a function unwrap that takes a value out of a box, 
-- and use fmap on that function to get your original box. Here’s how your code should work in GHCi:

-- GHCi> wrapped = fmap ? myBox
-- GHCi> wrapped
--  Box (Box 1)
-- GHCi> fmap unwrap wrapped
--  Box 1

myBox :: Box Int
myBox = Box 1

myBoxBox :: Box (Box Int)
myBoxBox = Box <$> myBox

unwrap :: Box (Box Int) -> Box Int
unwrap = fmap (\(Box v) -> v)


