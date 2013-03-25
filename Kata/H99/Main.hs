-- Main.hs
import H9901
import H9902
import H9903
import H9904
import H9905
import H9906
import H9907
import H9908
import H9909
import H9910
import H9911
import H9912
import H9913
import H9914
import H9915
import H9916
import H9917
import H9918
import H9919
import H9920
import H99from21to28
import H99from31to41

showOff :: (Show a) => a -> IO ()
showOff = putStrLn . show

main :: IO ()
main = do
  showOff $ myLast ['x', 'y', 'z']
  showOff $ myButLast ['w', 'x', 'y', 'z']
  showOff $ elementAt "haskell" 5
  showOff $ myLength "Hello, world!"
  showOff $ myReverse ['a'..'z']
  showOff $ isPalindrome "madamimadam"
  showOff $ myFlatten [['a'..'c'], ['d'..'f'], ['g'..'k']]
  showOff $ compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
  showOff $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
  showOff $ encode "aaaabccaadeeee"
  showOff $ encode1 "aaaabccaadeeee"
  showOff $ decode1 [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
  showOff $ encode2 "aaaabccaadeeee"
  showOff $ dupli ['a'..'d']
  showOff $ repli ['a'..'d'] 5
  showOff $ dropEvery ['a'..'k'] 3
  showOff $ split "howdy" 3
  showOff $ slice ['a'..'k'] 3 7
  showOff $ rotateL ['a'..'h'] (-2)
  showOff $ removeAt 2 "abcd"
  showOff $ insertAt 'X' "abcd" 2
  showOff $ range 4 9
  some <- rndSelect "abcdefgh" 3
  showOff some
  more <- diffSelect 6 49
  showOff more
  showOff $ isPrime (7::Int)
  showOff $ isPrime (10::Int)
  showOff $ myGCD (1989::Int) (867::Int)
  showOff $ coprime (35::Int) (64::Int)
  showOff $ totient (10::Int)
  showOff $ primeFactors (315::Int)
  showOff $ primesR (10::Int) (20::Int)
  showOff $ goldbach 1234
  showOff $ goldbachList 9 20