{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO

-- Q23.1 Rewrite the hello_world.hs program (reproduced here) from lesson 21 to use Text instead of String types.
-- helloPerson :: String -> String
-- helloPerson name = "Hello" ++ " " ++ name ++ "!"
 
-- main :: IO ()
-- main = do
--    putStrLn "Hello! What's your name?"
--    name <- getLineg 
--    let statement = helloPerson name
--    putStrLn statement

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello", " ", name, "!"]
 
main' :: IO ()
main' = do
   putStrLn "Hello! What's your name?"
   name <- TIO.getLine
   let statement = helloPerson name
   TIO.putStrLn statement

--Q23.2 Use Data.Text.Lazy and Data.Text.Lazy.IO to rewrite the lazy I/O section from lesson 22 by using the Text type.
--toInts :: T.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines
 
main :: IO ()
main = do
  userInput <- TLIO.getContents
  let numbers = toInts userInput
  print (sum numbers)