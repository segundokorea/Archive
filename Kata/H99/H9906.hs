module H9906 where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []      = False
isPalindrome [x,y]   = x == y
isPalindrome [x,_,z] = x == z
isPalindrome xs      = xs == reverse xs