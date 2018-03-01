binaryPartialApplication :: (a -> a -> a) -> a -> (a -> a)
binaryPartialApplication bf = bf