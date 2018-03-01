import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, newfile] -> do
      contents <- TIO.readFile file
      TIO.appendFile newfile contents
      putStrLn "Done"
    _ -> error "Wrong arguments"