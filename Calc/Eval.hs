module Eval where

import Syntax


-- Helpers for searching through the current environment
lookupExpr :: Name -> [Decl] -> Expr
lookupExpr s [] = Empty
lookupExpr s (d:env) =
  case d of
    Let t e   -> if s == t then e else lookupExpr s env
    Fun f x e -> if s == f then e else lookupExpr s env

lookupDecl :: Name -> [Decl] -> Decl
lookupDecl s (d:env) =
  case d of
    l@(Let t _)   -> if s == t then l else lookupDecl s env
    f@(Fun t _ _) -> if s == t then f else lookupDecl s env


-- Natural (i.e. non-conditional) expression evaluator
eval :: Expr -> [Decl] -> Int
eval (Id s)  env = eval (lookupExpr s env) env
eval (Nat n) env = n
eval (Neg e) env = -(eval e env)

-- Terms
eval (Mod e1 e2)   env = (eval e1 env) `mod` (eval e2 env)
eval (Div e1 e2)   env = (eval e1 env) `div` (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)

-- Generic expressions
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Plus e1 e2)  env = (eval e1 env) + (eval e2 env)

-- Function application
-- Substitute the function's formal parameter (n) with the value applied (x)
-- Because the substitution doesn't effect the environment, we get recursive functions for free
eval (App f x) env =
  case (lookupDecl f env) of
    Fun _ n e -> eval (subst n x e) env

-- Boolean expressions
eval (If cond e1 e2) env = if (ceval cond env) then (eval e1 env) else (eval e2 env)


-- Conditional expression evaluator
ceval :: Expr -> [Decl] -> Bool
ceval (Eq e1 e2) env = (eval e1 env) == (eval e2 env)
ceval (Ne e1 e2) env = (eval e1 env) /= (eval e2 env)
ceval (Gt e1 e2) env = (eval e1 env) >  (eval e2 env)
ceval (Ge e1 e2) env = (eval e1 env) >= (eval e2 env)
ceval (Lt e1 e2) env = (eval e1 env) <  (eval e2 env)
ceval (Le e1 e2) env = (eval e1 env) <= (eval e2 env)


-- Substitution helper
subst :: Name -> Expr -> Expr -> Expr
subst n x e@(Id y)
  | n == y    = x
  | otherwise = e
subst n x (Neg e) = Neg (subst n x e)
subst n x (Mod e1 e2) = Mod (subst n x e1) (subst n x e2)
subst n x (Div e1 e2) = Div (subst n x e1) (subst n x e2)
subst n x (Times e1 e2) = Times (subst n x e1) (subst n x e2)
subst n x (App f e) = App f (subst n x e)
subst n x (Eq e1 e2) = Eq (subst n x e1) (subst n x e2)
subst n x (Ne e1 e2) = Ne (subst n x e1) (subst n x e2)
subst n x (Gt e1 e2) = Gt (subst n x e1) (subst n x e2)
subst n x (Ge e1 e2) = Ge (subst n x e1) (subst n x e2)
subst n x (Lt e1 e2) = Lt (subst n x e1) (subst n x e2)
subst n x (Le e1 e2) = Le (subst n x e1) (subst n x e2)
subst n x (Minus e1 e2) = Minus (subst n x e1) (subst n x e2)
subst n x (Plus e1 e2) = Plus (subst n x e1) (subst n x e2)
subst n x (If cond e1 e2) = If (subst n x cond) (subst n x e1) (subst n x e2)
subst n x e = e
