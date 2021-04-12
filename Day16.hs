{-# LANGUAGE TupleSections #-}
module Day16 (process) where

import qualified Data.Text as T
import Data.Array.ST (STUArray, newListArray, readArray, writeArray, getElems, mapIndices)
import Data.Bits ((.&.))
import Data.Maybe (fromJust)
import Data.List (elemIndex, iterate)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (execStateT, get, gets, put, modify, StateT, lift)

data Action = Spin Int | Exchange Int Int | Partner Char Char

parseAction :: String -> Action
parseAction str = case str of
    's':rest -> Spin $ read rest
    ['p', f, '/', s] -> Partner f s
    'x':rest -> let (l, r) = break (=='/') rest in
        Exchange (read l) (read $ tail r)
    _ -> error $ "Bad action: " ++ str

type RunState s = (STUArray s Int Char, Int)

swap :: Int -> Int -> StateT (RunState s) (ST s) ()
swap i j = do
    let ri = i .&. 15
    let rj = j .&. 15
    (arr, x) <- get
    lift $ do
        a <- readArray arr ri
        readArray arr rj >>= writeArray arr ri
        writeArray arr rj a
    put (arr, x)

step :: Action -> StateT (RunState s) (ST s) ()
step (Spin n) = modify (fmap $ subtract n)
step (Exchange i j) = do
    d <- gets snd
    swap (i + d) (j + d)
step (Partner l r) = do
    arr <- lift <$> getElems =<< gets fst
    let f = fromJust $ elemIndex l arr
    let s = fromJust $ elemIndex r arr
    swap f s

mapFinal :: RunState s -> ST s String
mapFinal (arr, shift) = mapIndices (0, 15) ((.&.15) . (+shift)) arr >>= getElems

runArray :: [Action] -> String -> String
runArray actions start = runST $ (, 0) <$> array >>= execStateT (mapM_ step actions) >>= mapFinal
    where array = newListArray (0, 15) start :: ST s (STUArray s Int Char)

process :: [T.Text] -> (String, String)
process (text:_) = (final, final2)
    where actions = map (parseAction . T.unpack) $ T.split (==',') text
          base = ['a'..'p']
          generator = iterate (runArray actions) base
          final = head generator
          cycleLen = 1 + fromJust (elemIndex base $ tail generator)
          final2 = generator !! (1000000000 `mod` cycleLen)
process _ = undefined
