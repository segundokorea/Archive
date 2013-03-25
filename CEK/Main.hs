import CEK
import Parser
import System.IO

main = do
  putStr ">> "
  hFlush stdout
  l <- getLine
  case (parseExpr l) of
    Left err -> print err
    Right expr -> do
      let result = c $ eval expr
      putStr "=> "
      print result
  main

