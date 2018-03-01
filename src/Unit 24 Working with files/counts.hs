import System.Environment
import System.IO

getCount :: String -> (Int, Int, Int)
getCount input = (cCount, wCount, lCount)
  where cCount = length input
        wCount = length . words $ input
        lCount = length . lines $ input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =  
  unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

main :: IO ()
main = do 
  args <- getArgs
  let filename = head args
  file <- openFile filename ReadMode
  input <- hGetContents file
  let summary = (countsText . getCount) input
  putStrLn summary
  hClose file
  appendFile "stats.dat" (mconcat [filename, " ", summary, "\n"])
  putStrLn summary