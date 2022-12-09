{- Parser code -}

module Parse where
import System.IO (Handle, hGetContents, stdin)
import Lex
import Struct
import Helpers (splitWhen, lams, pairs, guardIO)

-- Throws a parser error message (s) at a certain position (p)
parseErr' p s = Left (p, s)

eofPos = (-1, 0)
eofErr = parseErr' eofPos "unexpected EOF"

-- Throws a parser error message (s) at the current position
parseErr s = ParseM $ \ ts ->
  let p = case ts of [] -> eofPos; ((p, _) : ts) -> p in
    Left (p, s)

-- Parse error message formatting
formatParseErr (line, col) emsg = Left $
  "Parse error at line " ++ show line ++
    ", column " ++ show col ++ ": " ++ emsg

-- Parsing monad: given the lexed tokens, returns either an error or (a, remaining tokens)
newtype ParseM a = ParseM ([(Pos, Token)] -> Either (Pos, String) (a, [(Pos, Token)]))

-- Extract the function from ParseM
parseMf (ParseM f) = f

-- Call a ParseM's function with some tokens
parseMt ts (ParseM f) = f ts

-- Given something and a list of tokens, return them in the ParseM monad
parseMr = curry Right

-- Try to parse the second arg, falling back to the first if fails
parseElse a' (ParseM a) =
  ParseM $ \ ts -> either (\ _ -> Right (a', ts)) Right (a ts)

-- ParseM instances:
instance Functor ParseM where
  fmap f (ParseM g) = ParseM $ \ ts -> g ts >>= \ p -> Right (f (fst p), snd p)

instance Applicative ParseM where
  pure = ParseM . parseMr
  ParseM f <*> ParseM g =
    ParseM $ \ ts -> f ts >>= \ p ->
    g (snd p) >>= \ p' ->
    Right (fst p (fst p'), snd p')

instance Monad ParseM where
  (ParseM f) >>= g = ParseM $ \ ts -> f ts >>= \ (a, ts') -> parseMf (g a) ts'

-- Peek at the next n tokens without consuming them
parsePeeks :: Int -> ParseM [Token]
parsePeeks n = ParseM $ \ ts -> if length ts < n then eofErr else parseMr [t | (_, t) <- take n ts] ts

-- Peek at the next token without consuming it
parsePeek :: ParseM Token
parsePeek = head <$> parsePeeks 1

-- Drop the next token
parseEat :: ParseM ()
parseEat = ParseM $ \ ts -> case ts of
  [] -> eofErr
  (_ : ts') -> Right ((), ts')

-- Consume token t.
parseDrop t = parsePeek >>= \ t' ->
  if t == t' then parseEat else parseErr ("expecting " ++ show t)

-- Consume token t if there is one.
-- (can't use parsePeek because there could be an optional EOF token ';')
parseDropSoft t = ParseM $ \ ts -> case ts of
  ((_, t') : ts') -> parseMr () (if t == t' then ts' else ts)
  [] -> parseMr () ts

parseVar :: ParseM String
parseVar = parsePeek >>= \ t -> case t of
  TkVar v -> parseEat *> pure v
  _ -> parseErr (if t `elem` keywords then show t ++ " is a reserved keyword"
                  else "expected a variable name here")

-- Zero or more vars
parseVars' :: ParseM [String]
parseVars' = parsePeek >>= \ t -> case t of
  TkVar v -> pure ((:) v) <* parseEat <*> parseVars'
  _ -> pure []

-- One or more vars
parseVars = pure (:) <*> parseVar <*> parseVars'

parseTerm1 :: ParseM Term
parseTerm1 = parsePeek >>= \ t -> case t of
  TkLam -> parseEat *> pure lams <*> parseVars <* parseDrop TkDot <*> parseTerm1
  TkLet -> parseEat *> pure (\ x t1 t2 -> App (Lam x t2) t1) <*> parseVar <* parseDrop TkEq
             <*> parseTerm1 <* parseDrop TkIn <*> parseTerm1
  _ -> parseTerm2

parseTerm2 :: ParseM Term
parseTerm2 = parseTerm3

parseTerm3 :: ParseM Term
parseTerm3 = parseTerm4

parseTerm4 :: ParseM Term
parseTerm4 = parseTerm5 >>= parseTermApp

parseTermApp :: Term -> ParseM Term
parseTermApp acc =
  parseElse acc $ parseTerm5 >>= parseTermApp . App acc

parseTerm5 :: ParseM Term
parseTerm5 = parsePeek >>= \ t -> case t of
  TkVar v -> parseEat *> pure (Var v)
  TkParenL -> parseEat *> parseTerm1 <* parseDrop TkParenR
  _ -> parseErr "couldn't parse a term here; perhaps add parentheses?"

{-
-- Program
parseDef :: ParseM (Maybe TermDef)
parseDef = parseElse Nothing $ parsePeek >>= \ t -> case t of
-- x vs... = term;
  TkVar x ->
    Just <$> (pure (TermDef x) <* parseEat
         <*> (pure (flip (foldr Lam)) <*> parseVars <* parseDrop TkEq <*> parseTerm1) <* parseDrop TkSemicolon)
  _ -> parseErr "you shouldn't see this"

parseDefsUntil :: ParseM [TermDef]
parseDefsUntil = parseDef >>= maybe (pure []) (\ p -> pure ((:) p) <*> parseDefsUntil)
-}

-- Program
parseDef :: ParseM TermDef
parseDef =
  -- x vs... = t
  pure TermDef <*> parseVar <*> (pure lams <*> parseVars' <* parseDrop TkEq <*> parseTerm1 <* parseDrop TkEOF)

parseDefs :: ParseM [TermDef]
parseDefs =
  ParseM $ \ toks ->
    -- split by unindented newline
    let dts = splitWhen (\ ((_, c), _) -> c == 1) toks in
      --error (unlines (map (show . addEOF) dts))
      mapM (parseOut' (parseDef) . addEOF) dts >>= \ ds -> Right (ds, [])

parseProgram :: ParseM Program
parseProgram = pure Program <*> parseDefs -- parseDefsUntil

parseFormatErr :: [(Pos, Token)] -> Either (Pos, String) a -> Either String a
parseFormatErr ts (Left (p, emsg))
  | p == eofPos = formatParseErr (fst (last ts)) emsg
  | otherwise = formatParseErr p emsg
parseFormatErr ts (Right a) = Right a

parseOut' :: ParseM a -> [(Pos, Token)] -> Either (Pos, String) a
parseOut' m ts =
  parseMf m ts >>= \ (a, ts') ->
  if length ts' == 0
    then Right a
    else parseErr' (fst $ head $ drop (length ts - length ts' - 1) ts)
           "couldn't parse after this"

-- Extract the value from a ParseM, if it consumed all tokens
parseOut :: ParseM a -> [(Pos, Token)] -> Either String a
parseOut m ts = parseFormatErr ts (parseOut' m ts)

-- Parse a whole program
parseFile :: [(Pos, Token)] -> Either String Program
parseFile = parseOut parseProgram

parseFile' :: String -> IO Program
parseFile' fn =
  guardIO (readFile fn >>= \ text -> return (lexFile text >>= parseFile))

parseFiles :: [String] -> IO Program
parseFiles [] = return (Program [])
parseFiles (fn : fns) =
  parseFile' fn >>= \ (Program ds) ->
  parseFiles fns >>= \ (Program ds') ->
  return (Program (ds ++ ds'))

-- Parse a term
parseTerm :: String -> Either String Term
parseTerm s = lexStr s >>= parseOut parseTerm1

-- Read a term at a time from a file stream, performing some action on each
readTerms' :: Handle -> (Term -> String) -> IO ()
readTerms' h f =
  filter (not . null) <$> lines <$> hGetContents h >>= \ ls ->
  foldr (\ s rest i -> putStrLn (either id f (lexStrL s i >>= parseOut parseTerm1)) >> rest (succ i)) (\ _ -> return ()) ls 1

readTerms = readTerms' stdin

-- Read two terms at a time from a file stream, performing some action on each pair
readTwoTerms' :: Handle -> (Term -> Term -> String) -> IO ()
readTwoTerms' h f =
  map (\ (st, su) -> st >>= \ t -> su >>= \ u -> return (t, u)) <$> pairs <$>
    map (\ (i, s) -> lexStrL s i >>= parseOut parseTerm1) <$>
    zip [1..] <$> lines <$> hGetContents h >>=
  foldr (\ ts rest -> putStrLn (either id (uncurry f) ts) >> rest) (return ())

readTwoTerms = readTwoTerms' stdin
