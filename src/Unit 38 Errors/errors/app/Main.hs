module Main where

import Lib

main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  print . displayResult . isPrime $ n