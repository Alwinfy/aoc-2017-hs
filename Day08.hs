module Day08 (process) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type RegisterState = (Int, Map.Map String Int)

data Insn = Insn String (Int -> Bool) String Int

mkpred :: String -> Int -> Int -> Bool
mkpred comparison rhs =
    case comparison of
        "==" -> (== rhs)
        "!=" -> (/= rhs)
        ">=" -> (>= rhs)
        "<=" -> (<= rhs)
        ">" -> (> rhs)
        "<" -> (< rhs)
        other -> error ("Bad comparison: " ++ other)

value :: String -> Int -> Int
value name val =
    case name of
        "inc" -> val
        "dec" -> -val
        other -> error ("Bad insn: " ++ other)

parse :: [String] -> Insn
parse [write, insn, val, "if", check, comparison, rhs] =
    Insn check (mkpred comparison $ read rhs) write (value insn $ read val)
parse other = error ("Bad insn string: " ++ show other)

runInsn :: RegisterState -> Insn -> RegisterState
runInsn (maxi, registers) (Insn check cond write change)
    | cond val  = (max maxi val, Map.insertWith (+) write change registers)
    | otherwise = (maxi, registers)
        where val = fromMaybe 0 $ Map.lookup check registers

process :: [T.Text] -> (String, String)
process text = let parsed = map (parse . map T.unpack . T.words) text in
    let (maxi, run) = foldl runInsn (0, Map.empty) parsed in
    (show . maximum $ Map.elems run, show maxi)
