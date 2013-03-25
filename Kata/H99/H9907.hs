module H9907 where

myFlatten :: [[a]] -> [a]
myFlatten = foldl1 (++)