module H9919 where

rotateL :: [a] -> Int -> [a]
rotateL l 0 = l
rotateL l n =
  let
    rotateL1 [] = []
    rotateL1 (x:xs) = xs ++ [x]
    rotateR1 [] = []
    rotateR1 l = (reverse . rotateL1 . reverse) l
  in
    if n > 0 then
      rotateL (rotateL1 l) (n-1)
    else -- n < 0
      rotateL (rotateR1 l) (n+1)