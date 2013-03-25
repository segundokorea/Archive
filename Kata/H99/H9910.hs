module H9910 where

encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode (z:zs) = reverse $ encode' zs (z,1) [] where
  encode' [] m res          = m:res
  encode' (x:xs) (y, n) res =
    if x == y then encode' xs (y, n+1) res
    else encode' xs (x,1) ((y, n):res)