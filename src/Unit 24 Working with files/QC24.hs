import System.IO

-- Quick check 24.2 Write the code to check whether the second line is empty before writing it to a file.
main :: IO ()
main = do
   helloFile <- openFile "hello.txt" ReadMode
   hasLine <- hIsEOF helloFile
   firstLine <- if not hasLine
                then hGetLine helloFile
                else return "empty"
   isEmpty <- hIsEOF helloFile
   secondLine  <- if not isEmpty
                  then do
                    secondLine <- hGetLine helloFile
                    anotherFile <- openFile "goodby.txt" WriteMode
                    hPutStrLn anotherFile secondLine
                  else
                    return ()
   putStrLn "done!"
