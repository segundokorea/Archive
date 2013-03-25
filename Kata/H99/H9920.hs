module H9920 where

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt 0 []     = Nothing
removeAt 0 (x:xs) = Just (x, xs)
removeAt n l =
  case removeAt 0 (drop (n-1) l) of
    Nothing      -> Nothing
    Just (x, xs) -> Just (x, (take (n-1) l) ++ xs)