-- Quick check 5.1 Write a function genIfXEven that creates a closure with x 
-- and returns a new function that allows the user to pass in a function to apply to x if x is even.
genIfXISEven :: (Integral n) => n -> (n -> n) -> n
genIfXISEven x = \f -> if evem x then f x else x