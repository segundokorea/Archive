module H9916 where

dropEvery :: [a] -> Int -> [a]
dropEvery l k = dropEvery' l 1 where
  dropEvery' [] _ = []
  dropEvery' (x:xs) n
    | n `mod` k == 0 = dropEvery' xs (n+1)
    | otherwise      = x : (dropEvery' xs (n+1))