module Day01 (process) where

import qualified Data.Text as T

val :: Char -> Char -> Int
val a b = if a == b then fromEnum a - fromEnum '0' else 0

countAdj :: String -> Int -> Int
countAdj list offset = sum $ zipWith val (drop offset $ cycle list) list

process :: [T.Text] -> (String, String)
process text = let joined = T.unpack $ T.concat text in
    (show $ countAdj joined 1, show . countAdj joined $ length joined `quot` 2)
