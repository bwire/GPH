{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random
import Glitcher

-- Quick check 25.1 Write a function that takes numbers in ASCII character form and converts them to Ints. 
--For example, make the following an Int:
-- bcInt :: BC.ByteString
-- bcInt = "6"

bsToInt :: BC.ByteString -> Int
bsToInt = read . BC.unpack

bcInt :: BC.ByteString
bcInt = "6"

-- Quick check 25.3 Write an IO action that returns a random Char
randomChar :: IO Char
randomChar = do
  char <- randomRIO (fromEnum (minBound :: Char), fromEnum (maxBound :: Char))
  return (toEnum char)

-- Quick check 25.4 Create a variable glitchActions outside your main that includes all your actions in a list. 
-- Don’t forget to give it the correct type.
actions :: [BC.ByteString -> IO BC.ByteString]
actions = [randomReplaceByte, randomSortSection, randomReplaceByte, randomSortSection, randomReplaceByte] 

-- Q25.2 Add another glitching technique, randomReverseBytes, that randomly reverses a section of bytes in your data.
reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [startpart, sortedchunk, restpart]
  where (startpart, end) = BC.splitAt start bytes
        (chunk, restpart) = BC.splitAt size end
        sortedchunk = BC.reverse $ chunk

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection bytes = do
  let sectionsize = 25
  let byteslength = BC.length bytes
  start <- randomRIO (0, byteslength - sectionsize)
  return $ reverseSection start sectionsize bytes


