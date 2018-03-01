import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  contents <- TIO.readFile file
  TIO.writeFile file $ T.toUpper contents
  putStrLn "Done"