module Day10 (process, knot) where

import qualified Data.Text as T
import qualified Data.Array.ST as Array
import Control.Monad.ST (ST, runST)
import Control.Monad.State (StateT, get, put, execStateT, lift, forM_)
import Data.Bits (xor)
import Data.List (splitAt)
import Data.Word (Word8)
import Text.Printf (printf)

type RollState s = (Word8, Word8, Array.STUArray s Word8 Word8)

runAction :: Word8 -> StateT (RollState s) (ST s) ()
runAction len = do
    (skip, pos, array) <- get
    let end = pos + len - 1
    forM_ [0..(len - 1) `quot` 2] $ lift . \i -> do
        let fpos = pos + i
        let epos = end - i
        first <- Array.readArray array fpos
        second <- Array.readArray array epos
        Array.writeArray array epos first
        Array.writeArray array fpos second
    put (skip + 1, pos + len + skip, array)

runActions :: [Word8] -> [Word8]
runActions rotations = runST $ do
    array <- Array.newListArray (0, 255) $ reverse [0..255]
    (_, _, result) <- execStateT (mapM_ runAction rotations) (0, 0, array)
    Array.getElems result

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = let (h, t) = splitAt n l
    in h : group n t

knot :: String -> [Word8]
knot string = map (foldl1 xor) . group 16 $ runActions seed
    where seed = concat . replicate 64 $ map (fromIntegral . fromEnum) string ++ [17, 31, 73, 47, 23]

process :: [T.Text] -> (String, String)
process (text:_) =
    let lens    = map (read . T.unpack) $ T.split (== ',') text in
        let vals  = map fromIntegral $ runActions lens :: [Int]
            vals2 = knot (T.unpack text) in
            (show $ head vals * (vals !! 1), concatMap (printf "%02x") vals2)
process _ = undefined
