module Glitcher where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random
import Control.Monad (foldM)

intToChar :: Int -> Char
intToChar i = toEnum safeInt
  where safeInt = i `mod` 255

intToBC :: Int -> BC.ByteString
intToBC =  BC.pack . (:[]) . intToChar

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc val bytes = mconcat [before, newChar, after]
  where (before, rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC val 

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return $ replaceByte location charVal bytes

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [startpart, sortedchunk, restpart]
  where (startpart, end) = BC.splitAt start bytes
        (chunk, restpart) = BC.splitAt size end
        sortedchunk = BC.reverse $ BC.sort chunk

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionsize = 25
  let byteslength = BC.length bytes
  start <- randomRIO (0, byteslength - sectionsize)
  return $ sortSection start sectionsize bytes

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

