module H9913 where
import H9911
encode2 :: (Eq a) => [a] -> [RunLength a]
encode2 = encode1