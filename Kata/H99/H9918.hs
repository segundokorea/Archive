module H9918 where

slice :: [a] -> Int -> Int -> [a]
slice l n m = take (m-n+1) l' where
  l' = drop (n-1) l