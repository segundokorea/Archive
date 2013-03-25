module BKTree (BKTree, mkBKTree, search) where
import Data.List (foldl')

data BKTree = BKTree {
  value :: String,
  children :: [(Int, BKTree)]
} deriving (Eq, Show)

-- Thanks to http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Haskell
levenshtein sa sb = last $ foldl transform [0..length sa] sb where
  transform xs@(x:xs') c = scanl compute (x+1) (zip3 sa xs xs') where
    compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

mkBKTree :: [String] -> BKTree
mkBKTree (s:ss) = foldl' insert (mkBKNode s) ss

mkBKNode :: String -> BKTree
mkBKNode s = BKTree s []

insert :: BKTree -> String -> BKTree
insert (BKTree t cs) s = BKTree t (c':res) where
  d  = levenshtein t s
  c  = filter ((== d) . fst) cs
  c' = case c of
    (_,b):_ -> (d, insert b s)
    _ -> (d, mkBKNode s) 
  res = filter ((/= d) . fst) cs

search :: BKTree -> String -> Int -> [String]
search b s n = map (value . snd) (search' b) where
  search' :: BKTree -> [(Int, BKTree)]
  search' b@(BKTree t cs) = if n < 0 then [] else b' ++ cs'' where
    d    = levenshtein t s
    b'   = if d <= n then [(d,b)] else []
    cs'  = filter (\(d',_) -> d-n <= d' && d' <= d+n) cs 
    cs'' = concat $ map (search' . snd) cs'
