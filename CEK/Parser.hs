module Parser (parseExpr) where
import Syntax
import Text.ParserCombinators.Parsec


parseExpr = parse expr ""

expr :: Parser Expr
expr = choice [nil, boolean, number, absE, ifE, letE, var, app]

nil :: Parser Expr
nil = do
  try $ string "nil"
  return Nil

boolean :: Parser Expr
boolean = choice [trueE, falseE]

trueE :: Parser Expr
trueE = do
  try $ string "true"
  return $ Boolean True

falseE :: Parser Expr
falseE = do
  try $ string "false"
  return $ Boolean False

number :: Parser Expr
number = do
  digits <- many1 digit
  return $ Number (read digits)

ifE :: Parser Expr
ifE = do
  try $ string "if"
  many1 space
  cond <- expr
  many1 space
  try $ string "then"
  many1 space
  e1 <- expr
  many1 space
  try $ string "else"
  many1 space
  e2 <- expr
  return $ If cond e1 e2

letE :: Parser Expr
letE = do
  try $ string "let"
  many1 space
  name <- var
  many space
  try $ string "="
  many space
  val <- expr
  many1 space
  try $ string "in"
  many space
  body <- expr
  return $ Let name val body

var :: Parser Expr
var = do
  c <- letter
  cs <- many alphaNum
  return $ Var (c:cs)

absE:: Parser Expr
absE= do
  try $ string "\\"
  many space
  param <- var
  many space
  try $ string "."
  many space
  body <- expr
  return $ Abs (Lambda (varName param) body)
  where
    varName (Var v) = v

app = do
  char '['
  func <- expr
  many1 space
  arg  <- expr
  many space
  char ']'
  return $ App func arg

