-- The infamous Fizz Buzz!
main :: IO ()
main = do
  let outs = fizzBuzz 1 100
  mapM_ putStrLn outs

fizzBuzz :: Int -> Int -> [String]
fizzBuzz n m = map stringify [n..m] where
  stringify :: Int -> String
  stringify a =
    if a `mod` 3 == 0 &&  a `mod` 5 == 0 then "FizzBuzz"
    else if a `mod` 5 == 0 then "Buzz"
    else if a `mod` 3 == 0 then "Fizz"
    else show a