import BKTree
import Control.Monad
import System.IO
import System.Random
import System.Environment
import Data.List.Split (splitOn)

randomize :: [a] -> IO [a]
randomize [] = return []
randomize xs = do
  let n = length xs
  i <- getStdRandom $ randomR (0,n-1)
  let (left, e:right) = splitAt i xs
  resR <- randomize right
  resL <- randomize left
  return (e:(resR ++ resL))

main :: IO ()
main = do
  args <- getArgs
  dict <- readFile "dict.txt"
  words <- randomize $ splitOn "\n" dict
  let term = args !! 0
  let fuzz = if length args < 2 then 2 else (read $ args !! 1)
  let results = search (mkBKTree words) term fuzz
  forM_ results putStrLn
