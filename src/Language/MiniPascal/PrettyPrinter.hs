module Language.MiniPascal.PrettyPrinter (prettyPrint) where

import Control.Monad (replicateM, void)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Traversable (for)
import Language.MiniPascal.Lang

data Ctx = Ctx
  { identLevel :: Int,
    -- skip ';' on next block
    skipSemicolon :: Bool
  }

newtype PrettyPrinter a = PrettyPrinter {unPrettyPrinter :: ReaderT Ctx (Writer String) a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader Ctx, MonadWriter String)

data NoExpr = NoExpr

data NoStm = NoStm

type instance Lit PrettyPrinter = String

type instance Expr PrettyPrinter = NoExpr

type instance Stm PrettyPrinter = NoStm

symbol :: String -> PrettyPrinter ()
symbol = tell

symbolShow :: Show a => a -> PrettyPrinter ()
symbolShow = symbol . show

forIndexed :: Monad m => [a] -> (Int -> a -> m b) -> m [b]
forIndexed xs handle =
  for (zip [0 ..] xs) \(i, x) ->
    handle i x

newline :: PrettyPrinter ()
newline = symbol "\n"

ident :: PrettyPrinter ()
ident = do
  level <- asks identLevel
  symbol $ mconcat $ replicate level "  "

line :: PrettyPrinter a -> PrettyPrinter a
line content = ident *> content <* newline

statement :: PrettyPrinter a -> PrettyPrinter a
statement content = ident *> content <* symbol ";" <* newline

nested :: PrettyPrinter a -> PrettyPrinter ()
nested block =
  local (\ctx -> ctx {identLevel = identLevel ctx + 1}) do
    void block

withoutSemicolon :: PrettyPrinter a -> PrettyPrinter a
withoutSemicolon = local (\ctx -> ctx {skipSemicolon = True})

withSemicolon :: PrettyPrinter a -> PrettyPrinter a
withSemicolon = local (\ctx -> ctx {skipSemicolon = False})

trimWhitespaces :: PrettyPrinter a -> PrettyPrinter a
trimWhitespaces = censor (reverse . dropWhile isSpace . reverse . dropWhile isSpace)
  where
    isSpace = (`elem` [' ', '\n'])

parens :: PrettyPrinter a -> PrettyPrinter a
parens item = symbol "(" *> item <* symbol ")"

sepBy :: [PrettyPrinter a] -> PrettyPrinter b -> PrettyPrinter [a]
sepBy items sep = do
  let isLast index = index == length items - 1
  forIndexed items \index item ->
    item <* unless (isLast index) (void sep)

instance MonadLang PrettyPrinter where
  lit x = symbol x $> NoExpr
  intLit = show
  realLit = show
  booleanLit True = "true"
  booleanLit False = "false"
  stringLit str = "'" <> (init . tail . show) str <> "'"
  deref x = symbolShow x $> NoExpr
  binop op a b = parens (a >> symbol " " >> symbolShow op >> symbol " " >> b) $> NoExpr
  unop op a = parens (a >> symbolShow op) $> NoExpr

  call function arguments = do
    symbolShow function
    parens $ arguments `sepBy` symbol ", "
    pure NoExpr

  block statements = do
    line $ symbol "begin"
    withSemicolon $ nested $ sequence statements
    skip <- asks skipSemicolon
    (if skip then line else statement) $ symbol "end"
    pure NoStm

  assign variable value = do
    statement do
      symbolShow variable
      symbol " := "
      value
      pure NoStm

  readln variables = do
    statement $ call "readln" $ deref <$> variables
    pure NoStm

  writeln values = do
    statement $ call "writeln" values
    pure NoStm

  if_ cond then_ else_ = do
    line $ symbol "if " *> cond *> symbol " then"
    line do
      trimWhitespaces $ withoutSemicolon then_
      symbol " else "
      trimWhitespaces $ else_

  while_ cond body = do
    line $ symbol "while " *> cond <* symbol " do"
    body

printVariables :: [(Variable, Type, String)] -> PrettyPrinter ()
printVariables [] = pure ()
printVariables variables = do
  line $ symbol "var"
  nested do
    for variables \(variable, type_, value) -> do
      statement do
        symbolShow variable
        symbol " : "
        symbolShow type_
        symbol " = "
        symbol value

printFunction :: Function PrettyPrinter -> PrettyPrinter ()
printFunction Function {name, arguments, type_, variables, body} = do
  statement do
    symbol "function " *> symbolShow name
    let printArgument (var, type_) = do
          symbolShow var
          symbol " : "
          symbolShow type_
    parens $ fmap printArgument arguments `sepBy` symbol "; "
    symbol " : "
    symbolShow type_
  printVariables variables
  body
  newline

printModule :: Module PrettyPrinter -> PrettyPrinter ()
printModule Module {variables, functions, body} = do
  printVariables variables
  unless (null variables) newline
  for functions printFunction
  line do
    trimWhitespaces $ withoutSemicolon body
    symbol "."

prettyPrint :: Module PrettyPrinter -> String
prettyPrint = execWriter . flip runReaderT (Ctx 0 False) . unPrettyPrinter . printModule
