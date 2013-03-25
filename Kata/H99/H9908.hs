module H9908 where

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (z:zs) = reverse $ compress' zs z [] where
  compress' [] y res     = y:res
  compress' (x:xs) y res =
    if x == y then compress' xs y res
    else compress' xs x (y:res)