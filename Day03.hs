module Day03 (process) where

import qualified Data.Text as T
--import Control.Monad.State

modulate :: [Int]
modulate = 9 : map (+2) modulate

keyframes :: [Int]
keyframes = 2 : 4 : 6 : 8 : zipWith (+) modulate keyframes

dist :: Int -> Int
dist 1 = 0
dist n = let (took, next:_) = span (<= n) keyframes in
     1 + min ((length took - 1) `quot` 4 + n - last took) (length took `quot` 4 + next - n)

process :: [T.Text] -> (String, String)
process (text:_) = let num = read $ T.unpack text in
    (show $ dist num, "TODO")
process _ = undefined
