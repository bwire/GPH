import Test.QuickCheck
import Test.QuickCheck.Instances
import Lib


assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

main :: IO ()
main = do
  quickCheck prop_punctuationInvariant
  quickCheck prop_inverseInvariant
  putStrLn "done!"

