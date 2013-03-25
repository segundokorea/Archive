module H9904 where

myLength :: [a] -> Int
myLength = foldl (\n _ -> n + 1) 0