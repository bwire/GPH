-- Q21.1 Translate listing 21.1 (reproduced below) into code by using do-notation in a Maybe. 
-- Assume that all the user input is replaced with a Map with a value for the input. 
-- Ignore the first putStrLn and simply return the statement at the end.

import qualified Data.Map as Map

userInput :: Map.Map Int String
userInput = Map.fromList [(1, "Barbed")] 

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"
 
maybeMain :: Int -> Maybe String
maybeMain n = do
   name <- Map.lookup n userInput
   return $ helloPerson name


-- Q21.2 Create a program that asks the user to input a number and then returns the nth 
-- Fibonacci numbers (see lesson 8 for an example of computing Fibonacci numbers).
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = f 1 1 n where
  f _ n2 2 = n2
  f n1 n2 c = 
    let s = n1 + n2
    in f n2 s (c - 1)

main :: IO ()
main = do
  putStrLn "Enter number"
  n <- getLine
  let num = read n :: Int
  putStrLn $ "The result is - " ++ (show . fib $ num)