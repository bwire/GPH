{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

-- Quick check 23.1 Create fourthWord once again, making the String type T.Text
firstWord :: String
firstWord = "pessimism"
 
secondWord :: T.Text
secondWord = T.pack firstWord
 
thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- Quick check 23.3 Create your own version of T.lines and T.unlines by using splitOn and T.intercalate.
lines' :: T.Text -> [T.Text]
lines' = T.splitOn "\n"

unlines' :: [T.Text] -> T.Text
unlines' = T.intercalate "\n"
