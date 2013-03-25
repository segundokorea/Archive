module H9911 where

data RunLength a = Multiple Int a | Single a deriving (Eq, Show)

encode1 :: (Eq a) => [a] -> [RunLength a]
encode1 []     = []
encode1 (z:zs) = reverse $ encode1' zs (Single z) [] where
  encode1' :: (Eq a) => [a] -> RunLength a -> [RunLength a] -> [RunLength a]
  encode1' [] r res = r:res
  encode1' (x:xs) r@(Single y) res
    | x == y    = encode1' xs (Multiple 2 y) res
    | otherwise = encode1' xs (Single x) (r:res)
  encode1' (x:xs) r@(Multiple n y) res
    | x == y    = encode1' xs (Multiple (n+1) y) res
    | otherwise = encode1' xs (Single x) (r:res)