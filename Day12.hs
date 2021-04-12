module Day12 (process) where

import qualified Data.Text as T
import qualified Data.Array as Array
import qualified Data.Set as Set

process :: [T.Text] -> (String, String)

parseNum :: T.Text -> Int
parseNum = read . T.unpack

readLine :: T.Text -> (Int, [Int])
readLine val = case T.splitOn (T.pack " <-> ") val of
    [l, r] -> (parseNum l, map parseNum $ T.splitOn (T.pack ", ") r)
    _ -> error $ "Bad line: " ++ show val

scanComponent :: Array.Array Int [Int] -> Set.Set Int -> Int -> Set.Set Int
scanComponent graph seen pos
    | Set.member pos seen = seen
    | otherwise = foldl (scanComponent graph) (Set.insert pos seen) (graph Array.! pos)

countComponentsR :: Array.Array Int [Int] -> Set.Set Int -> Int -> Int
countComponentsR graph set acc
    | Set.null set = acc
    | otherwise = let found = scanComponent graph Set.empty $ Set.findMin set in
        countComponentsR graph (set Set.\\ found) (acc + 1)

countComponents :: Array.Array Int [Int] -> Int
countComponents graph = countComponentsR graph (Set.fromAscList . Array.range $ Array.bounds graph) 0

process text = (show . Set.size $ scanComponent array Set.empty 0, show $ countComponents array)
    where array = Array.array (0, 1999) $ map readLine text
