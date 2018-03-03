-- Q25.1 Write a program that reads in a text file and outputs the difference between the number of characters 
-- in the file and the number of bytes in the file.
import System.Environment
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  bytes <- BC.readFile $ head args
  putStrLn $ mconcat 
    ["Number of bytes: ", 
     show (BC.length bytes), 
     "\nNumber of charcters: ", 
     show (T.length . E.decodeUtf8 $ bytes)]