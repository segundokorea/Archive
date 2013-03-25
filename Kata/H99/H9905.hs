module H9905 where

myReverse :: [a] -> [a]
myReverse xs = reverse' xs [] where
  reverse' [] res     = res
  reverse' (y:ys) res = reverse' ys (y:res)