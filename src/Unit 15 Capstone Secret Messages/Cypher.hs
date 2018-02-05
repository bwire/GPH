import RotCipher
import OneTimePad
import StreamCipher

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

instance Cipher Rot where
  encode Rot = rotEncoder
  decode Rot = rotDecoder

instance Cipher OneTimePad where
  encode (OTP key) = applyOTP $ cycle key
  decode (OTP key) = applyOTP $ cycle key

instance Cipher StreamCipher where
  encode (StreamCipher ln seed) = applyOTP $ cycle $ generateKey seed ln
  decode (StreamCipher ln seed) = applyOTP $ cycle $ generateKey seed ln