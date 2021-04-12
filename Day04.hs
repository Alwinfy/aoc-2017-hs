module Day04 (process) where

import qualified Data.Text as T
import Data.List (sort)

allUniq :: (Ord a) => [a] -> Bool
allUniq l = and $ zipWith (/=) list (tail list)
    where list = sort l

process :: [T.Text] -> (String, String)
process text = let rows = map T.words text in
    (show . length $ filter allUniq rows, show . length $ filter (allUniq . map (sort . T.unpack)) rows)
