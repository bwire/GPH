-- Quick check 17.1 Implement myAny by using function composition. 
-- myAny tests that a property is True for at least one value in the list.

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = not . null . filter (==True) . map f

-- Quick check 17.2 Can you use (/) to make Int a Semigroup?
-- No, because the result of (/) can be not Int - it doesn't satisfy the signature of (<>)