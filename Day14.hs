module Day14 (process) where

import qualified Data.Text as T
import qualified Data.Array.Unboxed as Array
import qualified Data.Set as Set
import Data.Word (Word8)
import Data.Bits (popCount, testBit)
import Day10 (knot)

type Pos = (Int, Int)
type Map = Array.UArray Pos Bool

toArray :: [[Word8]] -> Map
toArray list = Array.listArray ((0, 0), (127, 127)) $ concat bitList
    where bitList = map ((flip map [7, 6..0] . testBit) =<<) list

adjList :: Pos -> [Pos]
adjList (a, b) = [(a, b + 1), (a, b - 1), (a + 1, b), (a - 1, b)]

ref :: Map -> Pos -> Bool
ref arr pos =
    Array.inRange (Array.bounds arr) pos && arr Array.! pos

markAdj :: Map -> Set.Set Pos -> Pos -> Set.Set Pos
markAdj array seen pos
    | Set.member pos seen = seen
    | otherwise = foldl (markAdj array) (Set.insert pos seen) . filter (ref array) $ adjList pos

countComponents :: Map -> Int
countComponents array =
    fst $ foldl countOne (0, Set.empty) $ Array.range $ Array.bounds array
    where countOne (cnt, seen) pos
            | array Array.! pos && Set.notMember pos seen = (cnt + 1, markAdj array seen pos)
            | otherwise = (cnt, seen)

process :: [T.Text] -> (String, String)
process (seed:_) = (show . sum $ map (sum . map popCount) knots, show . countComponents $ toArray knots)
    where str = T.unpack seed ++ "-"
          range = [0..127] :: [Int]
          knots = map (knot . (str++) . show) range
process _ = undefined
