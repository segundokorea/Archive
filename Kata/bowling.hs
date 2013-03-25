type Roll  = Int          -- Number of pins knocked down
type Frame = (Roll, Roll) -- Two rolls per regular frame
                          -- In the case of a strike, use -1 to denote the second roll
                          -- A better implementation might use Maybe or an actual datatype

-- This example game should yield a score of 133
main :: IO ()
main = do
  let frames     = [(1,4), (4,5), (6,4), (5,5), (10,-1), (0,1), (7,3), (6,4), (10,-1), (2,8)]
  let extraRolls = [6]
  putStrLn $ show $ score frames extraRolls

-- Score a set of frames and any extra rolls
score :: [Frame] -> [Roll] -> Int
score frames extraRolls = score' frames 0 where
  -- Convert extra rolls into extra frames for ease of computation
  extraFrames = zip extraRolls $ repeat (-1)

  -- Grab the sum of the next two rolls
  nextTwoRolls ((r1,-1):f) = r1 + nextRoll f -- No second roll (strike or extra)
  nextTwoRolls ((r1,r2):_) = r1 + r2
  nextTwoRolls _ = 0

  -- Grab the next roll
  nextRoll ((r1,_):_) = r1
  nextRoll _ = 0

  -- Meat of the scoring algorithm
  score' :: [(Int, Int)] -> Int -> Int
  score' [] acc = acc
  score' ((r1,r2):fs) acc
    | r1 == 10      = score' fs $ acc + 10 + nextTwoRolls (fs++extraFrames) -- Strike
    | r1 + r2 == 10 = score' fs $ acc + 10 + nextRoll (fs++extraFrames)     -- Spare
    | otherwise     = score' fs $ acc + r1 + r2