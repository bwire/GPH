import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getCount :: T.Text -> (Int, Int, Int)
getCount input = (cCount, wCount, lCount)
  where cCount = T.length input
        wCount = length . T.words $ input
        lCount = length . T.lines $ input

countsText :: (Int, Int, Int) -> T.Text
countsText (cc, wc, lc) =  
  T.pack . unwords $ ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

main :: IO ()
main = do 
  args <- getArgs
  let filename = head args
  input <- TIO.readFile filename
  let summary = (countsText . getCount) input
  TIO.appendFile "stats.dat" (mconcat [T.pack filename, T.pack " ", summary, T.pack "\n"])
  TIO.putStrLn summary