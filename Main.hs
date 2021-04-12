import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import Text.Printf (printf)

dayfun :: Int -> [T.Text] -> (String, String)
dayfun n = case n of
    1  -> Day01.process
    2  -> Day02.process
    3  -> Day03.process
    4  -> Day04.process
    5  -> Day05.process
    6  -> Day06.process
    7  -> Day07.process
    8  -> Day08.process
    9  -> Day09.process
    10 -> Day10.process
    11 -> Day11.process
    12 -> Day12.process
    13 -> Day13.process
    14 -> Day14.process
    15 -> Day15.process
    16 -> Day16.process
    17 -> Day17.process
    _  -> error "Unimplemented day!"

perform :: Int -> IO ()
perform day = do
    file <- T.lines <$> T.IO.readFile path
    let (l, r) = dayfun day file
    putStrLn $ printf "%2d => %20s, %32s" day l r
    where path = printf "data/day%02d.in" day

main :: IO ()
main = mapM_ perform [1..17]
