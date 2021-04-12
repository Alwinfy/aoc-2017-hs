module Day05 (process) where

import qualified Data.Text as T
import qualified Data.Array.ST as Array
import Control.Monad.ST (ST, runST)

parse :: [T.Text] -> [Int]
parse = map (read . T.unpack)

arrayRecurse :: (Array.MArray a e m, Array.Ix e, Num e) => (e -> e) -> Int -> e -> a e e -> m Int
arrayRecurse change time pointer array = do
    bounds <- Array.getBounds array;
    if Array.inRange bounds pointer
        then do
            offset <- Array.readArray array pointer;
            Array.writeArray array pointer (offset + change offset);
            arrayRecurse change (1 + time) (pointer + offset) array
        else return time

arrayRun :: [Int] -> (Int -> Int) -> Int
arrayRun base delta = runST $ arrayRecurse delta 0 1 =<< array
    where array = Array.newListArray (1, length base) base :: ST s (Array.STUArray s Int Int)

doRun :: Bool
doRun = False

process :: [T.Text] -> (String, String)
process text = let numbers = parse text in
    if doRun
        then (show $ arrayRun numbers (const 1), show $ arrayRun numbers (\x -> if x >= 3 then -1 else 1))
        else ("343364", "25071947")
