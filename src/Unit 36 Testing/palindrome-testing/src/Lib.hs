module Lib
( 
  isPalindrome,
  preprocess,
  prop_punctuationInvariant,
  prop_inverseInvariant
) where

import Data.Char (isPunctuation)
import Data.Text as T

preprocess :: T.Text -> T.Text
preprocess text = T.filter (not . isPunctuation) text

prop_punctuationInvariant :: T.Text -> Bool 
prop_punctuationInvariant text = 
  preprocess text == preprocess noPuncText
    where noPuncText = T.filter (not . isPunctuation) text

prop_inverseInvariant :: T.Text -> Bool
prop_inverseInvariant = (==) <$> isPalindrome <*> isPalindrome . T.reverse

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
