module Main where

import Glitcher
import qualified Data.ByteString.Char8 as BC
import System.Environment
import Control.Monad (foldM)

main' :: IO ()
main' = do
  args <- getArgs
  let filename = head args
  imageFile <- BC.readFile filename
  --glitched <- randomReplaceByte imageFile
  glitched <- randomSortSection imageFile
  let glFileName = mconcat ["glitched_1_", filename]
  BC.writeFile glFileName glitched
  print "all done"

-- group transformation technic
main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  imageFile <- BC.readFile filename

  glitched <- foldM (\bytes func -> func bytes) 
    imageFile 
    [randomReplaceByte, randomSortSection, randomReplaceByte, randomSortSection, randomReplaceByte]

  let glFileName = mconcat ["glitched_2_", filename]
  BC.writeFile glFileName glitched
  print "all done"

