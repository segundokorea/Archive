module H9901 where

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast xs = Just $ head $ reverse xs