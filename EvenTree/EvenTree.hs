import Data.List (nub)
import System.IO
type Vertex = Int
type Edge   = (Vertex, Vertex)
type Edges  = [Edge]
type Tree   = Vertex -> [Vertex]

main = do
  input <- getContents
  let edges = tail $ parseInput input
  let tree = insert edges $ \v -> []
  print $ length $ filter (== True) $ map (canRemoveFrom tree) edges

canRemoveFrom :: Tree -> Edge -> Bool
canRemoveFrom t e@(a,b) = odd . length $ reachable (remove t e) a

insert :: Edges -> Tree -> Tree
insert [] t = t
insert ((a,b):es) t = insert es $ \v ->
  if v == a then b:(t a)
  else if v == b then a:(t b)
  else t v

remove :: Tree -> Edge -> Tree
remove t (a,b) = \v ->
  if v == a then filter (/= b) (t a)
  else if v == b then filter (/= a) (t b)
  else t v

reachable :: Tree -> Vertex -> [Vertex]
reachable t v = nub $ reachable' t [] v where
  reachable' :: Tree -> [Vertex] -> Vertex -> [Vertex]
  reachable' t visited v =
    if v `elem` visited then []
    else let
      ds = t v
      vs = concat $ ds : (map (reachable' t (v:visited)) ds)
    in filter (/= v) vs

parseInput :: String -> Edges
parseInput "" = []
parseInput input = (read a, read b) : (parseInput in2) where
  (_, in0)   = span (== ' ') input
  (a, _:in1) = span (/= ' ') in0
  (b, in2)   = span (/= '\n') in1
