module Token where
  
type Name = String


data Token

  -- Identifiers and numbers
  = ID Name
  | NAT Int

  -- Basic operations
  | NEG
  | PLUS | MINUS
  | TIMES | DIV | MOD

  -- Relational operators
  | ISEQUAL | ISNTEQUAL | LESS | LESSEQ | GREATER | GREATEREQ

  -- Declarations
  | LET | FUN | EQUAL

  -- Conditional expressions
  | IF | THEN | ELSE

  -- Punctuation
  | LPAR | RPAR

  deriving (Eq, Show)
