module H9909 where

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (z:zs) = reverse $ pack' zs [z] [] where
  pack' _ [] _       = []
  pack' [] m res     = m:res
  pack' (x:xs) (y:ys) res =
    if x == y then pack' xs (x:y:ys) res
    else pack' xs [x] ((y:ys):res)