import Parser(parse, stmt)
import Lexer(perform, tokenize)
import Eval
import Syntax
import System.IO

main :: IO ()
main = do
  calc []
  return ()


-- Where the magic happens...
-- Assume each line of input is a statement
calc :: [Decl] -> IO [Decl]
calc env = do

  -- Step 1: Get something to evaluate from the user
  putStr "$> "
  hFlush stdout
  l <- getLine

  -- Step 2: Tokenize the input string
  case (perform tokenize l) of
    Just (toks, res) -> do
      -- -- Debugging
      -- putStrLn $ ">> " ++ (show toks)
      -- if res /= "" then putStrLn $ "-> " ++ (show res) else return ()

      -- Step 3: Parse the input tokens
      case (parse stmt toks) of
        Just (st, res') -> do
          -- -- Debugging
          -- putStrLn $ ">> " ++ (show st)
          -- if res' /= [] then putStrLn $ "-> " ++ (show res') else return ()

          -- Step 4:
          -- For expressions, go ahead and evaluate, printing the result
          -- For declarations, update the current environment, printing the identifier
          case st of
            E e -> do
              putStrLn $ "=> " ++ show (eval e env)
              calc env
            D d -> do
              putStrLn $ "<= " ++ (getName d)
              calc (d : env)

        Nothing -> return env

    Nothing -> return env

-- Helper to get name of a declaration (i.e. its identifier)
getName :: Decl -> Name
getName (Let n _)   = n
getName (Fun n _ _) = n
