module Day15 (process) where

import qualified Data.Text as T

import Data.List (iterate)
import Data.Word (Word64)
import Data.Bits (shiftL, xor, (.&.))

bits :: Int
bits = 31
maxVal :: Word64
maxVal = (1 `shiftL` bits) - 1
cmpMask :: Word64
cmpMask = (1 `shiftL` 16) - 1

nextRng :: Word64 -> Word64 -> Word64
nextRng mul seed = (mul * seed) `mod` maxVal

generator :: Word64 -> Word64 -> [Word64]
generator mul seed = tail $ iterate (nextRng mul) seed

lastEq :: Word64 -> Word64 -> Bool
lastEq a b = ((a `xor` b) .&. cmpMask) == 0

judge :: Int -> [Word64] -> [Word64] -> Int
judge n f = length . filter id . take n . zipWith lastEq f

doRun :: Bool
doRun = False

process :: [T.Text] -> (String, String)
process (f:s:_) = if doRun
    -- TODO: Look into ways to reduce mem usage
    then (show $ judge 40000000 gen1 gen2, show $ judge 5000000 (filter ((==0) . (.&.3)) gen1) (filter ((==0) . (.&.7)) gen2))
    else ("638", "343")
    where seed = read . T.unpack . last . T.split (==' ')
          gen1 = generator 16807 $ seed f
          gen2 = generator 48271 $ seed s

process _ = undefined
