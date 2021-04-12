module Day06 (process) where

import qualified Data.Text as T
import qualified Data.Set as Set
import Control.Monad (forM_)
import Data.Array.ST (newListArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, bounds, (!), listArray, elems, range)
import Data.List (iterate, elemIndex)
import Data.Maybe (fromJust)
import Data.Word (Word8)

runList :: UArray Word8 Word8 -> UArray Word8 Word8
runList array = runSTUArray $ do
    let arange = bounds array;
    let arrayLen = 1 + snd arange;
    let (maxi, pos) = foldl (\(m, p) i -> if m < (array ! i) then (array ! i, i) else (m, p)) (0, 0) $ range arange;
    let add = maxi `div` arrayLen;
    let over = maxi `mod` arrayLen;
    arr <- newListArray (bounds array) . map (+add) $ elems array;
    writeArray arr pos add;
    forM_ [1+pos..pos+over] $ \i ->
        let p = i `mod` arrayLen in
            writeArray arr p =<< (+1) <$> readArray arr p
    return arr

findFirstR :: (Ord a) => Set.Set a -> Int -> [a] -> (Int, a)
findFirstR set time (h:t)
    | Set.member h set = (time, h)
    | otherwise        = findFirstR (Set.insert h set) (time + 1) t
-- Only ever used on an infinite list
findFirstR _ _ _ = undefined

findFirst :: (Ord a) => [a] -> (Int, a)
findFirst = findFirstR Set.empty 0
    

process :: [T.Text] -> (String, String)
process (text:_) = (show time, show $ time - fromJust (elemIndex item array))
    where list         = map (read . T.unpack) $ T.words text
          array        = iterate runList . listArray (0, fromIntegral $ length list - 1) $ list
          (time, item) = findFirst array
process _ = undefined
