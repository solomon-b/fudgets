module HbcWord(module HbcWord,Word) where
import Data.Word
import Data.Bits

--type Word=Word.Word32
--type Short=Word16

bitAnd x y = x .&. y
bitOr x y = x .|. y
bitXor x y = xor x y
bitRsh x y = shiftR x y
bitLsh x y = shiftL x y

intToWord = fromIntegral :: Int->Word
wordToInt = fromIntegral :: Word->Int
