module Syntax where


data Expr = Nil           -- Void, null, undefined, empty
          | Boolean Bool  -- True or false
          | Number Int    -- You know, integers
          | Var Name      -- Variable reference
          | Abs Lambda    -- Lambda abstraction
          | App Expr Expr -- Application
          | If Expr Expr Expr -- If-then-else
          | Let Expr Expr Expr -- Let-in

instance Show Expr where
  show Nil = "nil"
  show (Boolean True) = "true"
  show (Boolean False) = "false"
  show (Number n) = show n
  show (Abs l) = show l

  -- If you're showing these, you're probably mistaken
  show (Var v) = " ... "
  show (App _ _) = " ... "
  show (If _ _ _) = " ... "

type Name = String

data Lambda = Lambda Name Expr

instance Show Lambda where
  show (Lambda name expr) = "\\" ++ name ++ "." ++ show expr
