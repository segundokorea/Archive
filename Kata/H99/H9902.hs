module H9902 where

myButLast :: [a] -> Maybe a
myButLast []       = Nothing
myButLast [_]      = Nothing
myButLast [x,_]    = Just x
myButLast [_,x,_]  = Just x
myButLast (_:_:zs) = myButLast zs