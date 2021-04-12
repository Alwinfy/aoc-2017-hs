module Day09 (process) where

import qualified Data.Text as T
import qualified Control.Monad.State as State

data Datum = Garbage Int | Group [Datum]    
    deriving (Show)

type ReadState = State.State String

readGarbage :: Int -> ReadState Datum
readGarbage n = do
    first <- State.get
    case first of
        '!':_:rest -> State.put rest >> readGarbage n
        '>':rest -> State.put rest >> return (Garbage n)
        _:rest -> State.put rest >> readGarbage (n + 1)
        _ -> error "Read incomplete garbage!"

readGroups :: ReadState [Datum]
readGroups = do
    datum <- readHead
    first <- State.get
    case first of
        '}':rest -> State.put rest >> return [datum]
        ',':rest -> (datum:) <$> (State.put rest >> readGroups)
        _ -> error $ "Expected comma or }, got " ++ show first

readHead :: ReadState Datum
readHead = do
    first <- State.get
    case first of
        '<':rest -> State.put rest >> readGarbage 0
        '{':'}':rest -> State.put rest >> return (Group [])
        '{':rest -> Group <$> (State.put rest >> readGroups)
        _ -> error $ "Expected < or {, got " ++ show first

count :: Int -> Datum -> Int
count layer (Group children) = layer + sum (map (count $ layer + 1) children)
count _ _ = 0

countGarbage :: Datum -> Int
countGarbage (Garbage n) = n
countGarbage (Group children) = sum $ map countGarbage children

process :: [T.Text] -> (String, String)
process (text:_) = let datum = State.evalState readHead $ T.unpack text in
    (show $ count 1 datum, show $ countGarbage datum)
process _ = undefined
