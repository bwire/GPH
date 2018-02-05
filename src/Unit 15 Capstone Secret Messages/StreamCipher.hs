module StreamCipher (StreamCipher(..), generateKey) where

import OneTimePad (intToBits, bitsToChar)

type MaxNumber = Int
type Seed = Int
type KeyLength = Int

data StreamCipher = StreamCipher KeyLength Seed

prng :: Int -> Int -> MaxNumber -> Seed -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

generateKey :: Seed -> KeyLength -> String
generateKey _ 0 = [] 
generateKey seed l = 
  let nextSeed = getNextSeed seed
      token = bitsToChar $ intToBits nextSeed 
  in token : generateKey nextSeed (l - 1)

getNextSeed :: Seed -> Int
getNextSeed = prng 1337 7 1000

