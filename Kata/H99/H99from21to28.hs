{-# LANGUAGE DoAndIfThenElse #-}
module H99from21to28 where
import System.Random
import Control.Monad
import Data.Sequence ((<|),(|>))
import qualified Data.Sequence as S

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n =
  let
    (pre, post) = splitAt n xs
  in
    concat $ pre : [x] : [post]

range :: Int -> Int -> [Int]
range n m
  | n <= m    = n : (range (n+1) m)
  | otherwise = []

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  result <- forM [1..n] (\_ -> do
    i <- getStdRandom $ randomR (0,n-1)
    return (xs!!i))
  return result

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  result <- forM [1..n] (\_ -> do
    i <- getStdRandom $ randomR (1,m)
    return i)
  return result