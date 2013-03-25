module H99from31to41 where

isPrime :: (Integral a) => a -> Bool
isPrime n = not $ any (divBy n) [2..m] where
  m = floor $ sqrt $ (fromIntegral n :: Float)
  divBy a b = (a `mod` b) == 0

myGCD :: (Integral a) => a -> a -> a
myGCD a b
  | a == 0    = b
  | b == 0    = a
  | a < b     = myGCD a (b-a)
  | otherwise = myGCD (a-b) b

coprime :: (Integral a) => a -> a -> Bool
coprime a b = myGCD a b == 1

totient :: (Integral a) => a -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..n-1]

primeFactors :: (Integral a) => a -> [a]
primeFactors n = filter (\x -> n `divBy` x && isPrime x) [2..m] where
  m = floor $ sqrt $ (fromIntegral n :: Float)
  divBy a b = (a `mod` b) == 0

primesR :: Int -> Int -> [Int]
primesR n m = filter isPrime [n..m]

goldbach :: Int -> (Int, Int)
goldbach n = head $ [(x,y) | x <- primesR 2 (n-2), let y = n - x, isPrime y]

goldbachList :: Int -> Int -> [(Int, (Int, Int))]
goldbachList n m = [(x, goldbach x) | x <- evens n m] where
  evens x y = filter even [x..y]