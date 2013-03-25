-- A simple dice game with seemingly arbitrary rules

import System.Random

rollDice :: Int -> IO Int
rollDice _ = getStdRandom (randomR (1,6))

-- In this version we score five die rolls
main :: IO ()
main = do
  rolls <- mapM rollDice [1..5]
  putStrLn $ "Rolls: " ++ show rolls
  putStrLn $ "Score: " ++ show (score rolls)


-- Score any number of rolls
score :: [Int] -> Int
score rolls =
  scoreSingleOne    rolls +
  scoreSingleFive   rolls +
  scoreTripleOnes   rolls +
  scoreTripleTwos   rolls +
  scoreTripleThrees rolls +
  scoreTripleFours  rolls +
  scoreTripleFives  rolls +
  scoreTripleSixes  rolls
  where
    -- If there are c rolls of r, assign n points
    score' c r n rolls =
      let count x xs = length $ filter ((==) x) xs
      in if count r rolls == c then n else 0

    -- Assign points based on the rules...
    scoreSingleOne    = score' 1 1  100
    scoreSingleFive   = score' 1 5  50
    scoreTripleOnes   = score' 3 1  1000
    scoreTripleTwos   = score' 3 2  200
    scoreTripleThrees = score' 3 3  300
    scoreTripleFours  = score' 3 4  400
    scoreTripleFives  = score' 3 5  500
    scoreTripleSixes  = score' 3 6  600