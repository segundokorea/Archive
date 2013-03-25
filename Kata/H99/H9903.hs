module H9903 where

elementAt :: [a] -> Int -> Maybe a
elementAt xs n
  | n <= 0        = Nothing
  | n > length xs = Nothing
  | otherwise     = Just $ xs !! (n-1)