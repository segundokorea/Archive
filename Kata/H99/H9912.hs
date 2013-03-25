module H9912 where
import H9911

decode1 :: (Eq a) => [RunLength a] -> [a]
decode1 [] = []
decode1 (z:zs) = concat $ l:(decode1 zs):[] where
  l = case z of
    Single a     -> [a]
    Multiple n a -> take n $ repeat a