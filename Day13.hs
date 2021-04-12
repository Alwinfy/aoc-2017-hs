module Day13 (process) where

import qualified Data.Text as T

readLine :: T.Text -> (Int, Int)
readLine text = (l, r)
    where [l, r] = map (read . T.unpack) $ T.split (== ':') text

collides :: Int -> (Int, Int) -> Bool
collides offset (dist, cyc) = (offset + dist) `mod` (2 * cyc - 2) == 0

mul :: (Int, Int) -> Int
mul (x, y) = x * y

process :: [T.Text] -> (String, String)
process text = (show . sum . map mul . filter (collides 0) $ walls, show . head $ filter (not . flip any walls . collides) [0..])
    where walls = map readLine text
