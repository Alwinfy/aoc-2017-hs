module Day07 (process) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Control.Monad.State (State, get, gets, put, execState)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.List (sort)

data Node = Node {weight :: Int, children :: [Node]} deriving (Show, Eq, Ord)

makeNode :: Int -> Node
makeNode size = Node size []

updateNode :: [Node] -> Node -> Node
updateNode kids' (Node wgt kids) = Node (foldl (+) wgt (map weight kids')) $ kids ++ kids'

parseLine :: [T.Text] -> ((T.Text, Node), Maybe (T.Text, [T.Text]))
parseLine [name, l] = ((name, makeNode . read $ T.unpack l), Nothing)
parseLine (name:l:_:rest) = ((name, makeNode . read $ T.unpack l), Just (name, map (T.dropWhileEnd (== ',')) rest))
parseLine bad = error $ "Got bad line: " ++ show bad

type BuildState = (Map.Map T.Text Node, Map.Map T.Text [T.Text])

buildNode :: T.Text -> State BuildState ()
buildNode target = do
    kids <- gets $ fromMaybe [] . Map.lookup target . snd;
    mapM_ buildNode kids;
    (output, input) <- get;
    let kidless = foldl (flip Map.delete) output kids;
    let output' = Map.adjust (updateNode $ map (output Map.!) kids) target kidless;
    put (output', Map.delete target input)

buildNodes :: State BuildState ()
buildNodes = do
    todo <- gets snd;
    if Map.null todo
        then return ()
        else (buildNode . head $ Map.keys todo) >> buildNodes

genMap :: [T.Text] -> [(T.Text, Node)]
genMap texts = let (nodes, taskList) = unzip $ map (parseLine . T.words) texts in
    Map.assocs . fst . execState buildNodes $ (Map.fromList nodes, Map.fromList $ catMaybes taskList)

finalize :: Node -> Maybe Int -> Maybe Int
finalize node = fmap ((weight node - (sum . map weight $ children node)) -)

candidates :: Node -> Maybe Int -> Maybe Int
candidates node guess = case sort $ children node of
    [] -> finalize node guess
    [_] -> Nothing
    [a, b]
        | diff == 0 -> finalize node guess
        | otherwise -> candidates a (Just diff) <|> candidates b (Just (-diff))
            where diff = weight a - weight b
{-
        case guess of
            Nothing -> candidates a (Just diff) <|> candidates b (Just (-diff))
            Just val -> if val == diff then candidates a guess else 
                        if val == -diff then candidates b guess else Nothing
-}
    first:rest
        | weight first == weight final -> finalize node guess
        | weight secnd == weight final -> candidates first $ Just (weight first - weight final)
        | weight first == weight secnd -> candidates final $ Just (weight final - weight first)
        | otherwise -> undefined
            where final = last rest
                  secnd = head rest


process :: [T.Text] -> (String, String)
process text = let (name, root) = head $ genMap text in
    (T.unpack name, show . fromJust $ candidates root Nothing)
