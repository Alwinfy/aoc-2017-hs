module Day02 (process) where

import qualified Data.Text as T
import Data.Maybe (fromJust)
import Control.Monad (msum, liftM2)

parseRow :: T.Text -> [Int]
parseRow = map (read . T.unpack) . T.words

cksum :: [Int] -> Int
cksum list = maximum list - minimum list


divTest :: Int -> Int -> Maybe Int
divTest a b = if a > b && a `rem` b == 0
    then Just (a `quot` b)
    else Nothing

division :: [Int] -> Int
division list = fromJust . msum $ liftM2 divTest list list

process :: [T.Text] -> (String, String)
process text = let rows = map parseRow text in
    (show . sum $ map cksum rows, show . sum $ map division rows)
