{-data Token =
    TokVar String
  | TokLam
  | TokDot
  | TokLParen
  | TokRParen
  | TokEq
  | TokLet
  | TokIn

--lexVar :: String -> [Token] -> [Token]

--lex :: String -> [Token] -> [Token]
lex ('\\' : cs) acc = lex cs (TokLam : acc)
lex ('.' : cs) acc = lex cs (TokDot : acc)
lex ('(' : cs) acc = lex cs (TokLParen : acc)
lex (')' : cs) acc = lex cs (TokRParen : acc)
lex ('=' : cs) acc = lex cs (TokEq : acc)
lex 
-}

{- Lexer code -}

module Lex (Token (..), keywords, Pos, lexFile) where
import Data.Char (isAlpha, isDigit)

-- Possible tokens
data Token =
    TkVar String -- "x"
  | TkLam -- "\"
  | TkParenL -- "("
  | TkParenR -- ")"
  | TkEq -- "="
  | TkLet -- "let"
  | TkIn -- "in"
  | TkDot -- "."
  | TkSemicolon -- ";"
  | TkEOF
  deriving Eq

instance Show Token where
  show (TkVar x) = x
  -- Punctuation tokens
  show TkLam = "\\"
  show TkParenL = "("
  show TkParenR = ")"
  show TkEq = "="
  show TkDot = "."
  show TkSemicolon = ";"
  show TkEOF = "\0"
  -- Keyword tokens
  show TkLet = "let"
  show TkIn = "in"


type Pos = (Int, Int) -- Line, column

-- Increments column by n
forward' :: Int -> Pos -> Pos
forward' n (line, column) = (line, column + n)

-- Increments column by 1
forward :: Pos -> Pos
forward = forward' 1

-- Goes to the next line
next :: Pos -> Pos
next (line, column) = (succ line, 1)

-- List of punctuation tokens
punctuation = [TkLam, TkParenL, TkParenR, TkEq, TkDot, TkSemicolon]
-- List of keyword tokens (use alphanumeric chars)
keywords = [TkLet, TkIn]

-- Tries to lex s as punctuation, otherwise lexing s as a keyword or a var
lexPunctuation :: String -> Pos -> [(Pos, Token)] -> Either (Pos, String) [(Pos, Token)]
lexPunctuation s =
  foldr (\ t owise -> maybe owise (flip (lexAdd (show t)) t) (prefix (show t) s))
        (lexKeywordOrVar s)
        punctuation
  where
    prefix (tc : tcs) (c : cs) =
      if tc == c then prefix tcs cs else Nothing
    prefix [] s = Just s
    prefix _ [] = Nothing


-- Lex a string, returning a list of tokens
lexStrh :: String -> Pos -> [(Pos, Token)] -> Either (Pos, String) [(Pos, Token)]
lexStrh (' ' : s) = lexStrh s . forward
lexStrh ('\t' : s) = lexStrh s . (forward' 8)
lexStrh ('\r' : '\n' : s) = lexStrh s . next
lexStrh ('\n' : s) = lexStrh s . next
lexStrh ('\r' : s) = lexStrh s . next
lexStrh ('-' : '-' : s) = lexComment Nothing s
lexStrh ('{' : '-' : s) = lexComment (Just 0) s
lexStrh ('-' : '}' : s) = \ p _ -> Left (p, "unexpected end-of-comment '-}'")
lexStrh "" = \ p ts -> Right ((p, TkEOF) : ts)
lexStrh s = lexPunctuation s

-- Lex a comment.
-- lexComment Nothing scans a comment from -- to the end of the line.
-- lexComment (Just n) scans a nested comment (inside {- and -}).
lexComment :: Maybe Integer -> String -> Pos -> [(Pos, Token)] -> Either (Pos, String) [(Pos, Token)]
lexComment Nothing  ('\n' : s) = lexStrh s . next
lexComment (Just n) ('\n' : s) = lexComment (Just n) s . next
lexComment (Just 0) ('-' : '}' : s) = lexStrh s . forward' 2
lexComment (Just n) ('-' : '}' : s) = lexComment (Just (pred n)) s . forward' 2
lexComment (Just n) ('{' : '-' : s) = lexComment (Just (succ n)) s . forward' 2
lexComment multiline (_ : s) = lexComment multiline s . forward
lexComment _ "" = \ _ ts -> Right ts

-- Consumes characters until a non-variable character is reached
lexVar :: String -> (String, String)
lexVar "" = ("", "")
lexVar (c : s)
  | isVarChar c = h [c] s
  | otherwise = ("", (c : s))
  where
    h v "" = (reverse v, "")
    h v (c : s)
      | isVarChar c = h (c : v) s
      | otherwise = (reverse v, (c : s))

-- Determines if c is a valid character for a variable name
isVarChar :: Char -> Bool
isVarChar c = isAlpha c || isDigit c || c `elem` "_'?<>+-*&^%$#@!~|"

-- Lex a keyword or a variable name
lexKeywordOrVar :: String -> Pos -> [(Pos, Token)] -> Either (Pos, String) [(Pos, Token)]
lexKeywordOrVar s p ts =
  case lexVar s of
    (v@(_:_), rest) -> trykw keywords v rest p ts
    ("", next:_) -> Left (p, "unexpected character '" ++ [next] ++ "'")
    ("", "") -> Left (p, "unexpected end of file (this shouldn't happen)")
  where
    trykw (kwtok : kws) v s =
      if show kwtok == v then lexAdd v s kwtok else trykw kws v s
    trykw [] v s = lexAdd v s (TkVar v)

{- Add a token to output token list and continue lexing

- t_s: new token as string
- s: rest of string after t_s
- t: new token as Token
- p: position of t
- ts: token list -}

lexAdd :: String -> String -> Token -> Pos -> [(Pos, Token)] -> Either (Pos, String) [(Pos, Token)]
lexAdd t_s s t p ts = lexStrh s (forward' (length t_s) p) ((p, t) : ts)

-- Format for a lex error
lexErr :: (Pos, String) -> String
lexErr ((line, col), msg) = "error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg

-- Lex a string.
lexStr :: String -> Either String [(Pos, Token)]
lexStr s = either (Left . lexErr) (Right . reverse) $ lexStrh s (1, 1) []

-- Synonym for lexStr
lexFile = lexStr
