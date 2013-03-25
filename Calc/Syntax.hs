module Syntax where

type Name = String


data Expr
  = Empty

  -- Factors
  | Id Name
  | Nat Int
  | Neg Expr
  | App Name Expr

  -- Terms
  | Mod Expr Expr
  | Div Expr Expr
  | Times Expr Expr

  -- Relational expressions
  | Eq Expr Expr
  | Ne Expr Expr
  | Gt Expr Expr
  | Ge Expr Expr
  | Lt Expr Expr
  | Le Expr Expr

  -- Higher-precedence expressions
  | Minus Expr Expr
  | Plus Expr Expr

  -- Conditional expressions
  | If Expr Expr Expr

  deriving (Eq, Show)


data Decl
  = Let Name Expr
  | Fun Name Name Expr
  deriving (Eq, Show)

data Stmt = D Decl | E Expr deriving (Eq, Show)
