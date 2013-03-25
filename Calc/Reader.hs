module Reader where


data Reader a = R (String -> Maybe (a, String))

instance Monad Reader where
  return v = R (\instream -> Just (v, instream))

  R p >>= f = R $ \instream ->
    case p instream of
      Nothing             -> Nothing
      Just (v, instream') -> let R q = f v in q instream'

  fail s = R (\instream -> Nothing) 

-- Grab the first character from the input
getc :: Reader Char
getc = R $ \input -> 
  case input of
    []     -> Nothing
    (c:cs) -> Just (c, cs)

-- Monadic "or"
choice :: Reader a -> Reader a -> Reader a
choice (R p) (R q) = R $ \instream ->
  case p instream of
    Nothing -> q instream
    res     -> res

(+++) = choice

-- Kleene star (R*)
star :: Reader a -> Reader [a]
star r = starPlus r +++ return []

-- RR*
starPlus :: Reader a -> Reader [a]
starPlus r = r >>= (\v -> star r >>= (\vs -> return (v:vs)))

-- "Lookahead"
invert :: a -> Reader b -> Reader a
invert v (R r) = R $ \instream ->
  case r instream of
    Nothing -> Just (v, instream)
    Just _  -> Nothing

-- Satisfiability for input char
sat :: (Char -> Bool) -> Reader Char
sat prop = do
  c <- getc
  if prop c then return c else fail ""

-- Equivalence for input char
checkChar :: Char -> Reader Char
checkChar c = sat $ \c' -> c == c'

-- Equivalence for input string
checkString :: String -> Reader String
checkString "" = return ""
checkString s@(c:cs) = do
  checkChar c
  checkString cs
  return s
