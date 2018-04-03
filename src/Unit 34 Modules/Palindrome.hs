module Palindrome (isPalindrome) where

import qualified Data.Text as T
import Data.Char (toLower, isSpace, isPunctuation)

stripWhiteSpaces :: T.Text -> T.Text
stripWhiteSpaces = T.filter (not . isSpace)

stripPunctuations :: T.Text -> T.Text
stripPunctuations = T.filter (not . isPunctuation)

toLowerCase :: T.Text -> T.Text
toLowerCase = T.map toLower

preprocess :: T.Text -> T.Text
preprocess = toLowerCase . stripPunctuations . stripWhiteSpaces

isPalindrome :: T.Text -> Bool
isPalindrome = ((==) <$> T.reverse <*> id) . preprocess

