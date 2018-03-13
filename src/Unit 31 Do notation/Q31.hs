-- Q31.1 At the end of lesson 21, you saw the following program used to calculate the cost of pizza.Desugar this code to use >>=, >>, return and lambda functions rather than do-notation.
-- Desugar this code to use >>=, >>, return and lambda functions rather than do-notation.

comparePizzas :: (Int, Double) -> (Int, Double) -> (Int, Double)
comparePizzas = undefined
describePizza = undefined

main' :: IO ()
main' = do
   putStrLn "What is the size of pizza 1"
   size1 <- getLine
   putStrLn "What is the cost of pizza 1"
   cost1 <- getLine
   putStrLn "What is the size of pizza 2"
   size2 <-  getLine
   putStrLn "What is the cost of pizza 2"
   cost2 <- getLine
   let pizza1 = (read size1, read cost1)
   let pizza2 = (read size2, read cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   putStrLn (describePizza betterPizza)

main :: IO ()
main = putStrLn "What is the size of pizza 1" >>
   getLine >>=
   \size1 -> putStrLn "What is the cost of pizza 1" >>
   getLine >>=
   \cost1 -> putStrLn "What is the size of pizza 2" >>
   getLine >>=
   \size2 -> putStrLn "What is the cost of pizza 2" >>
   getLine >>=
   \cost2 -> putStrLn $ describePizza $ comparePizzas (read size1, read cost1) (read size2, read cost2)  

-- Q31.2 At the end of lesson 21 in unit 4, we first introduced the idea that do-notation isn’t specific to IO. 
-- You ended up with this function for a Maybe type:

-- maybeMain :: Maybe String
-- maybeMain = do
--    size1 <- Map.lookup 1 sizeData
--    cost1 <- Map.lookup 1 costData
--    size2 <- Map.lookup 2 sizeData
--    cost2 <- Map.lookup 2 costData
--    let pizza1 = (size1,cost1)
--    let pizza2 = (size2,cost2)
--    let betterPizza = comparePizzas pizza1 pizza2
--    return  (describePizza betterPizza)

-- Rewrite this function so it works with the List type (don’t worry if the results seem strange). 
sizeData = []
costData = []

listMain :: [String]
listMain = do
   size <- sizeData
   cost <- costData
   return $ describePizza (size, cost)   

-- Q31.3 Refactor the maybeMain function from the preceding exercise so that it works with any Monad. 
-- You’ll need to change the type signature as well as remove the type-specific parts from the body of the function.
maybeMain :: Monad m => m Int -> m Double -> m Int -> m Double -> m String
maybeMain msz1 mcost1 msz2 mcost2 = do
   size1 <- msz1
   cost1 <- mcost1
   size2 <- msz2
   cost2 <- mcost2
   let pizza1 = (size1, cost1)
   let pizza2 = (size2, cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   return  (describePizza betterPizza)
