import System.IO

main :: IO ()
main = do
  myFile <- openFile "./hello.txt" ReadMode
  firstLine <- hGetLine myFile
  putStrLn firstLine
  secondLine <- hGetLine myFile
  anotherFile <- openFile "./goodby.txt" WriteMode
  hPutStrLn anotherFile secondLine  
  hClose myFile
  hClose anotherFile
  putStrLn "done!"