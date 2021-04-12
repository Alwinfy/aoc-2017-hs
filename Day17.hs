module Day17 (process) where

import qualified Data.Text as T
import Control.Monad.ST
import Control.Monad (foldM)
import Data.STRef
import Data.Maybe (fromJust)

data MutCons s a = MutCons {car :: a, cdr :: (STRef s (Maybe (MutCons s a)))}
-- Head, ptr, len
type SpinState s = (MutCons s Int, MutCons s Int)

scanFwd :: Int -> SpinState s -> ST s (SpinState s)
scanFwd 0 state = return state
scanFwd n (first, ptr) = do
    deref <- readSTRef $ cdr ptr
    scanFwd (n - 1) $ case deref of
        Nothing -> (first, first)
        Just next -> (first, next)

findMut :: (Eq a) => a -> MutCons s a -> ST s (Maybe (MutCons s a))
findMut search cons
    | car cons == search = return (Just cons)
    | otherwise = do
        deref <- readSTRef $ cdr cons
        case deref of
            Nothing -> return Nothing
            Just next -> findMut search next

initState :: ST s (SpinState s)
initState = do
    base <- newSTRef Nothing
    let body = MutCons 0 base
    return (body, body)

insert :: Int -> SpinState s -> ST s (SpinState s)
insert n (h, ptr) = do
    rest <- readSTRef $ cdr ptr
    toInsert <- MutCons n <$> newSTRef rest
    writeSTRef (cdr ptr) $ Just toInsert
    return (h, toInsert)

doInserts :: Int -> [Int] -> SpinState s -> ST s (SpinState s)
doInserts cycleLen args start = foldM doInsert start args
    where doInsert st n = scanFwd (cycleLen `mod` n) st >>= insert n

extract :: SpinState s -> Int
extract = car . snd

process :: [T.Text] -> (String, String)
process (text:_) = (show $ runST $ extract <$> (pipeline 2017 >>= scanFwd 1), show $ runST $ extract <$> (pipeline 50000000 >>= (mapM $ fmap fromJust <$> findMut 0) >>= scanFwd 1))
    where pipeline n = initState >>= doInserts (read $ T.unpack text) [1..n]
process _ = undefined
