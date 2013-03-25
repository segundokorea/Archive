module Parser where

import Syntax
import Token
import Prelude hiding (id, mod, div)


data Parser a = P ([Token] -> Maybe (a, [Token]))

instance Monad Parser where
  return v = P (\toks -> Just (v, toks))

  P p >>= f = P $ \toks ->
    case p toks of
      Nothing -> Nothing
      Just (v, toks') -> let P q = f v in q toks'

  fail s = P (\_ -> Nothing)


-- Monadic extensions
choice :: Parser a -> Parser a -> Parser a
choice (P p) (P q) = P $ \x ->
  case p x of
    Nothing -> q x
    res     -> res

(+++) = choice

star :: Parser a -> Parser [a]
star r = starPlus r +++ return []

starPlus :: Parser a -> Parser [a]
starPlus r = r >>= (\v -> star r >>= (\vs -> return (v:vs)))


-- Token operations
getToken :: Parser Token
getToken = P $ \toks ->
  case toks of
    [] -> Nothing
    (tok:toks') -> Just (tok, toks')

isToken :: Token -> Parser ()
isToken t = do
  t' <- getToken
  if t == t' then return () else fail ("Couldn't match expected token '" ++ show t ++ "' with actual token '" ++ show t' ++ "'")


-- Factors
nat :: Parser Expr
nat = do
  t <- getToken
  case t of
    NAT n -> return (Nat n)
    otherwise -> fail ("Expected natural number from given token '" ++ show t ++ "'")

id :: Parser Expr
id = do
  t <- getToken
  case t of
    ID s -> return (Id s)
    otherwise -> fail ("Expected identifier from given token '" ++ show t ++ "'")

neg :: Parser Expr
neg = do
  isToken NEG
  e <- factor
  return (Neg e)

app :: Parser Expr
app = do
  f <- id
  isToken LPAR
  x <- expr
  isToken RPAR
  return (App (getId f) x)

factor :: Parser Expr
factor = nat +++ id +++
  (getToken >>= (\t ->
    case t of
      LPAR -> do
        f <- expr
        isToken RPAR
        return f
      otherwise -> fail ""))

signed :: Parser Expr
signed = neg +++ app +++ factor


-- Terms
mod :: Parser Expr
mod = do
  e1 <- signed
  isToken MOD
  e2 <- term
  return (Mod e1 e2)

div :: Parser Expr
div = do
  e1 <- signed
  isToken DIV
  e2 <- term
  return (Div e1 e2)

times :: Parser Expr
times = do
  e1 <- signed
  isToken TIMES
  e2 <- term
  return (Times e1 e2)

term :: Parser Expr
term = mod +++ div +++ times +++ signed


-- Generic expressons
minus :: Parser Expr
minus = do
  e1 <- term
  isToken MINUS
  e2 <- expr
  return (Minus e1 e2)

plus :: Parser Expr
plus = do
  e1 <- term
  isToken PLUS
  e2 <- expr
  return (Plus e1 e2)

expr :: Parser Expr
expr = plus +++ minus +++ term


-- Relational expressions
eq :: Parser Expr
eq = do
  e1 <- expr
  isToken ISEQUAL
  e2 <- expr
  return (Eq e1 e2)

ne :: Parser Expr
ne = do
  e1 <- expr
  isToken ISNTEQUAL
  e2 <- expr
  return (Ne e1 e2)

gt :: Parser Expr
gt = do
  e1 <- expr
  isToken GREATER
  e2 <- expr
  return (Gt e1 e2)

ge :: Parser Expr
ge = do
  e1 <- expr
  isToken GREATEREQ
  e2 <- expr
  return (Ge e1 e2)

lt :: Parser Expr
lt = do
  e1 <- expr
  isToken LESS
  e2 <- expr
  return (Lt e1 e2)

le :: Parser Expr
le = do
  e1 <- expr
  isToken LESSEQ
  e2 <- expr
  return (Le e1 e2)

rexpr :: Parser Expr
rexpr = eq +++ ne +++ gt +++ ge +++ lt +++ le


-- Conditional expressions
ifthenelse :: Parser Expr
ifthenelse = do
  isToken IF
  p  <- rexpr
  isToken THEN
  e1 <- expr
  isToken ELSE
  e2 <- expr
  return (If p e1 e2)

cexpr :: Parser Expr
cexpr = ifthenelse +++ expr


-- Declarations
getId :: Expr -> String
getId (Id n) = n
getId _ = ""

letDecl :: Parser Decl
letDecl = do
  v <- id
  isToken EQUAL
  e <- cexpr
  return (Let (getId v) e)

funDecl :: Parser Decl
funDecl = do
  f <- id
  x <- id
  isToken EQUAL
  e <- cexpr
  return (Fun (getId f) (getId x) e)

decl :: Parser Decl
decl = do
  t <- getToken
  case t of
    LET -> letDecl
    FUN -> funDecl
    otherwise -> fail ("Expected let- or function-declaration from given token '" ++ show t ++ "'")


-- Statements
stmt :: Parser Stmt
stmt = 
  (decl >>= (\d -> return (D d)))
  +++
  (cexpr >>= (\e -> return (E e)))


-- Like Lexer's perform
parse :: Parser a -> ([Token] -> Maybe (a, [Token]))
parse (P p) = p
