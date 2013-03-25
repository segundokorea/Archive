module H9917 where

split :: [a] -> Int -> ([a], [a])
split l k =
  if (k > 0) && (k < length l) then
    (take k l, drop k l)
  else ([], [])