module Day11 (process) where

import qualified Data.Text as T

type Coord = (Int, Int)

-- Convention: (x, y) -> (ne, n)
dir :: String -> Coord
dir s = case s of
    "n" -> (0, 1)
    "ne" -> (1, 0)
    "se" -> (1, -1)
    "s" -> (0, -1)
    "sw" -> (-1, 0)
    "nw" -> (-1, 1)
    _ -> error $ "Bad dir: " ++ s

dist :: Coord -> Int
dist (x, y) = max (abs $ x + y) $ max (abs x) (abs y)

add2 :: Coord -> Coord -> Coord
add2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

process :: [T.Text] -> (String, String)
process (text:_) = let coords = map (dir . T.unpack) $ T.split (== ',') text in
    (show . dist $ foldl1 add2 coords, show . maximum . map dist $ scanl1 add2 coords)
process _ = undefined
