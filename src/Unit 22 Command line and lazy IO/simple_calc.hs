-- Q22.1 Write a program, simple_calc.hs, that reads simple equations involving adding two numbers or multiplying two numbers. 
-- The program should solve the equation each user types into each line as each line is entered.
import Data.Char
import System.Exit
import Control.Monad.Trans.Except


parse :: String -> Int
parse s = 
  let [(firstNumber, rest)] = reads s 
      operator = head rest 
      [(secondNumber, _)] = reads . tail $ rest
  in case operator of
    '*' -> firstNumber * secondNumber
    '+' -> firstNumber + secondNumber

main' :: IO ()
main' = do
  input <- getLine
  print $ parse input 

-- Q22.2 Write a program that allows a user to select a number between 1 and 5 and then prints a famous quote 
-- (quotes are of your choosing). After printing the quote, the program will ask whether the user would like another. 
-- If the user enters n, the program ends; otherwise, the user gets another quote. The program repeats until the user enters n. 
-- Try to use lazy evaluation and treat the user input as a list rather than recursively calling main at the end.
quotes :: [String]
quotes = ["First quote", "Second quote", "Third quote", "Forth quote", "Fifth quote"]
 
getQuote :: Char -> String
getQuote c =  
  if notElem c "12345"
  then ""
  else quotes !! (read [c] - 1) 

action :: Char -> IO ()
action c = 
  if c == 'n' then
    exitSuccess
  else 
    do 
      let quote = getQuote c 
      if null quote
      then putStrLn "\nWrong input! Try again.."
      else do
        putStrLn ('\n' : quote)
        putStrLn "Try once more" 

-- I'm not truing to catch exit exception here!!
main :: IO ()
main = do
  putStrLn "Enter a number between 1 and 5"
  num <- getContents
  mapM_ action num
