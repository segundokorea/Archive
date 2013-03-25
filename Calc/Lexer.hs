module Lexer where

import Reader
import Token
import qualified Data.Char as Char


isAlpha :: Reader Char
isAlpha = sat Char.isAlpha

isAlphaNum :: Reader Char
isAlphaNum = sat Char.isAlphaNum

isDigit :: Reader Char
isDigit = sat Char.isDigit

isSpace :: Reader Char
isSpace = sat Char.isSpace

isntSpace :: Reader Char
isntSpace = sat (not . Char.isSpace)

ident :: Reader String
ident = do
  c  <- isAlpha
  cs <- star isAlphaNum
  return (c:cs)

nat :: Reader Int
nat = do
  cs <- starPlus isDigit
  return (read cs)

space :: Reader ()
space = do
  star isSpace
  return ()

token :: Reader a -> Reader a
token r = do
  space
  v <- r
  return v

identifier :: Reader String
identifier = token ident

natural :: Reader Int
natural = token nat

symbol :: String -> Reader String
symbol s = token $ checkString s

keyword :: String -> Reader String
keyword s = do
  v <- symbol s
  invert v isAlphaNum

tokenRdr :: Reader Token
tokenRdr =
  ((>>=) natural (\n -> return (NAT n)))
  +++
  ((>>=) (keyword "let") (\_ -> return LET))
  +++
  ((>>=) (keyword "fun") (\_ -> return FUN))
  +++
  ((>>=) (keyword "if") (\_ -> return IF))
  +++
  ((>>=) (keyword "then") (\_ -> return THEN))
  +++
  ((>>=) (keyword "else") (\_ -> return ELSE))
  +++
  ((>>=) identifier (\s -> return (ID s)))
  +++
  ((>>=) (symbol "==") (\_ -> return ISEQUAL))
  +++
  ((>>=) (symbol "!=") (\_ -> return ISNTEQUAL))
  +++
  ((>>=) (symbol "=") (\_ -> return EQUAL))
  +++
  ((>>=) (symbol "<=") (\_ -> return LESSEQ))
  +++
  ((>>=) (symbol "<") (\_ -> return LESS))
  +++
  ((>>=) (symbol ">=") (\_ -> return GREATEREQ))
  +++
  ((>>=) (symbol ">") (\_ -> return GREATER))
  +++
  ((>>=) (symbol "+") (\_ -> return PLUS))
  +++
  ((>>=) (symbol "-") (\_ -> return MINUS))
  +++
  ((>>=) (symbol "*") (\_ -> return TIMES))
  +++
  ((>>=) (symbol "/") (\_ -> return DIV))
  +++
  ((>>=) (symbol "%") (\_ -> return MOD))
  +++
  ((>>=) (symbol "~") (\_ -> return NEG))
  +++
  ((>>=) (symbol "(") (\_ -> return LPAR))
  +++
  ((>>=) (symbol ")") (\_ -> return RPAR))


tokenize :: Reader [Token]
tokenize = starPlus tokenRdr

perform :: Reader a -> (String -> Maybe (a, String))
perform (R p) = p
