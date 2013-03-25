-- Real credit to Matt Might, whose code I basically stole
-- http://matt.might.net/articles/cek-machines/

module CEK where
import Syntax


-- Simga - State space or configuration of the evaluator
data Sigma = S { -- Sigma commonly used in literature
  c :: Expr,     -- Code or control, the current expression
  e :: Env,      -- Environment, a map of declarations
  k :: Kont      -- Continuation, a stack of frames
}

type Env = Name -> Val

data Kont = Empty
          | Arg Expr Env Kont
          | Fun Lambda Env Kont
          | IfElse (Expr,Expr) Env Kont
          | LetIn Expr Expr Env Kont

data Val = NilVal
         | BooleanVal Bool
         | NumberVal Int
         | Clos (Lambda, Env)

type Program = Expr


-- Transition function
step :: Sigma -> Sigma

-- Look up a variable reference
step (S (Var x) e k) = S v e' k where
  (v, e') = case e x of
    NilVal -> (Nil, e)
    BooleanVal b -> (Boolean b, e)
    NumberVal n -> (Number n, e)
    Clos (l,e'') -> (Abs l, e'')

-- If expression, first eval condition
step (S (If cond expr1 expr2) e k) = S cond e (IfElse (expr1,expr2) e k)

-- Evaluate a let-expr by starting with the binding
step (S (Let name val body) e k) = S val e (LetIn name body e k)

-- Evaluate the binding, then the in-expr
step (S rep e (LetIn (Var name) body e' k)) = S body e'' k where
  e'' = e' // (name, val)
  val = valueOf rep e

-- Evaluate the appropriate branch of an if-expresion
step (S (Boolean b) e (IfElse (expr1, expr2) e' k))
  | b = S expr1 e' k
  | otherwise = S expr2 e' k

-- Evaluate the function, then its arguments
step (S (App f x) e k) = S f e (Arg x e k)

-- Next, evaluate arguments, then apply
step (S (Abs l) e (Arg x e' k)) = S x e' (Fun l e k)

-- Now perform the application
step (S arg e (Fun (Lambda param body) e' k)) = S body e'' k where
  e'' = e' // (param, val)
  val = valueOf arg e

valueOf :: Expr -> Env -> Val
valueOf Nil _ = NilVal
valueOf (Boolean b) _ = BooleanVal b
valueOf (Number n) _ = NumberVal n
valueOf (Abs l) e = Clos (l, e)

-- Mr. Might uses this to make his code look nice and mathy
-- But I don't really care all that much
(//) :: Eq a => (a -> b) -> (a,b) -> (a -> b)
(//) f (x,y) = \ x' -> if x == x' then y else f x'


-- Turn an expression into an evaluation stack
inject :: Program -> Sigma
inject c = S c e Empty where
  e = \x -> error $ "no binding for " ++ x

-- Decide if the stack has been evaluated
isFinal :: Sigma -> Bool
isFinal (S { c=Nil, k=Empty }) = True
isFinal (S { c=(Boolean b), k=Empty }) = True
isFinal (S { c=(Number _), k=Empty }) = True
isFinal (S { c=(Abs _), k=Empty }) = True
isFinal _ = False

-- Step over each state until isFinal satisfied
terminal :: (Sigma -> Sigma) -> (Sigma -> Bool) -> Sigma -> Sigma
terminal step isFinal s | isFinal s = s
                        | otherwise = terminal step isFinal (step s)

-- Load and evaluate a program
eval :: Program -> Sigma
eval p = terminal step isFinal (inject p)

