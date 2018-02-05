module OneTimePad (OneTimePad(..), applyOTP, intToBits, bitsToChar) where

data OneTimePad = OTP String

xorBool :: Bool -> Bool -> Bool
xorBool l r = (l || r) && (not (l && r))

xorPair :: (Bool, Bool) -> Bool
xorPair (val1, val2) = xorBool val1 val2

xor :: [Bool] -> [Bool] -> [Bool]
-- the same as: xor xs = map xorPair . zip xs
xor = ((map xorPair) .) . zip

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = 
  let n' = n `div` 2
      bit = n `mod` 2
  in (if bit == 0 then False else True) : intToBits' n'
  
maxBits' :: Int
maxBits' = length . intToBits' $ maxBound

intToBits n = 
  let 
    bits = reverse . intToBits' $ n
    lpref = maxBits' - length bits
    leadingFalses = take lpref $ repeat False
  in leadingFalses ++ bits
  
charToBits :: Char -> Bits
charToBits = intToBits . fromEnum

bitsToInt bits = 
  let lbits = length bits
      idxs = [lbits - 1, lbits - 2 .. 0]
      pairs = zip bits idxs
      ones = filter fst pairs
   in sum $ map ((2 ^) . snd) ones

bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInt

-- one-time pad functionality
applyOTP' :: String -> String -> [Bits]
applyOTP' txtInput txtKey = 
  let bitsInput = map charToBits txtInput
      bitsKey = map charToBits txtKey
      pairs = zip bitsInput bitsKey
  in map (\(bi, bk) -> bi `xor` bk) pairs

applyOTP :: String -> String -> String
applyOTP = (map bitsToChar .) . applyOTP'