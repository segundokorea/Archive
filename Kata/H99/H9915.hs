module H9915 where

repli :: [a] -> Int -> [a]
repli l n = concat $ map (\x -> take n $ repeat x) l