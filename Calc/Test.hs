import Token
import Syntax
import qualified Lexer as L
import qualified Parser as P
import qualified Eval as E
import Prelude


-- Lexer tests
tokenize = L.perform L.tokenize
testL0 = tokenize "1+2"    == Just ([NAT 1, PLUS, NAT 2], "")
testL1 = tokenize "xyz"    == Just ([ID "xyz"], "")
testL2 = tokenize "~+-*/%" == Just ([NEG, PLUS, MINUS, TIMES, DIV, MOD], "")
testL3 = tokenize "=="     == Just ([ISEQUAL], "")
testL4 = tokenize "="      == Just ([EQUAL], "")
testL5 = tokenize "!="     == Just ([ISNTEQUAL], "")
testL6 = tokenize ">"      == Just ([GREATER], "")
testL7 = tokenize ">="     == Just ([GREATEREQ], "")
testL8 = tokenize "func"   == Just ([ID "func"], "")
testL9 = tokenize "!"      == Nothing


-- Parser tests
parse = P.parse P.stmt
testP0 = parse [NAT 1, PLUS, NAT 2] == Just (E (Plus (Nat 1) (Nat 2)), [])
testP1 = parse [LET, ID "x", EQUAL, NAT 5] == Just (D (Let "x" (Nat 5)), [])
testP2 = parse [LET, ID "x", EQUAL] == Nothing
testP3 = parse [FUN, ID "succ", ID "x", EQUAL, ID "x", PLUS, NAT 1]
         == Just (D (Fun "succ" "x" (Plus (Id "x") (Nat 1))), [])
testP4 = parse [LET, ID "y", EQUAL, NAT 0, NAT 1] == Just (D (Let "y" (Nat 0)), [NAT 1])
testP5 = parse [ID "succ", LPAR, ID "x", RPAR] == Just (E (App "succ" (Id "x")), [])
testP6 = parse [ID "succ", LPAR, ID "x"] == Nothing


-- Evaluator tests
eval = E.eval
testE0 = eval (Id "x") [Fun "succ" "x" (Plus (Id "x") (Nat 1)), Let "x" (Nat 5)] == 5
testE1 = eval (App "succ" (Id "x")) [Fun "succ" "x" (Plus (Id "x") (Nat 1)), Let "x" (Nat 5)] == 6
testE2 = eval (App "fact" (Nat 5)) [Fun "fact" "n" (If (Gt (Id "n") (Nat 0)) (Times (Id "n") (App "fact" (Minus (Id "n") (Nat 1)))) (Nat 1))] == 120
testE3 = eval (App "fact" (Id "x")) [Fun "fact" "n" (If (Gt (Id "n") (Nat 0)) (Times (Id "n") (App "fact" (Minus (Id "n") (Nat 1)))) (Nat 1)), Let "x" (Nat 5)] == 120


-- Test runner
run :: String -> Bool -> IO ()
run name outcome = do
  putStr $ "Test '" ++ name ++ "'... "
  if outcome then putStrLn "passed" else putStrLn "failed"
  return ()

main :: IO ()
main = do
  run "Lexer Test #0" testL0
  run "Lexer Test #1" testL1
  run "Lexer Test #2" testL2
  run "Lexer Test #3" testL3
  run "Lexer Test #4" testL4
  run "Lexer Test #5" testL5
  run "Lexer Test #6" testL6
  run "Lexer Test #7" testL7
  run "Lexer Test #8" testL8
  run "Lexer Test #9" testL9
  run "Parser Test #0" testP0
  run "Parser Test #1" testP1
  run "Parser Test #2" testP2
  run "Parser Test #3" testP3
  run "Parser Test #4" testP4
  run "Parser Test #5" testP5
  run "Evaluator Test #0" testE0
  run "Evaluator Test #1" testE1
  run "Evaluator Test #2" testE2
  run "Evaluator Test #3" testE3
