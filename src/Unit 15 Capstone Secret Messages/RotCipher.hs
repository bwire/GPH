module RotCipher (Rot(..), rotEncoder, rotDecoder) where

data Rot = Rot

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN l v = 
  let hl = l `div` 2
      offset = hl + fromEnum v 
  in toEnum (offset `mod` l) 

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder l v = 
  let hl = l `div` 2 + if odd l then 1 else 0
      offset = hl + fromEnum v 
  in toEnum (offset `mod` l) 
  
alphabetSize :: Enum a => a -> Int
alphabetSize = (+1) . fromEnum 

charSetSize :: Int
charSetSize = alphabetSize(maxBound::Char)
 
rotChar :: Char -> Char
rotChar = rotN charSetSize

rotEncoder :: String -> String
rotEncoder = map . rotN $ charSetSize

rotDecoder :: String -> String
rotDecoder = map . rotNDecoder $ charSetSize
        
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map . rotN $ alphabetSize(maxBound::FourLetterAlphabet)
 
fourLetterAlphabetDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetDecoder = map . rotNDecoder $ alphabetSize(maxBound::FourLetterAlphabet)

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder =  map . rotN $ alphabetSize(maxBound::ThreeLetterAlphabet)

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder =  map . rotNDecoder $ alphabetSize(maxBound::ThreeLetterAlphabet)
