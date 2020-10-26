module Language.MiniPascal.Parser (parse) where

import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Language.MiniPascal.AST (toAST)
import qualified Language.MiniPascal.Lang as Lang
import Language.MiniPascal.PrettyPrinter
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "{" "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

call :: Parser a -> Parser (String, [a])
call arg = do
  name <- identifier
  arguments <- parens (arg `sepBy` symbol ",")
  pure (name, arguments)

variable :: Parser Lang.Variable
variable = (\name -> Lang.Variable {name, annotation = Nothing}) <$> identifier

literal :: Lang.MonadLang m => Parser (Lang.Lit m)
literal =
  choice
    [ try $ Lang.realLit <$> L.signed sc (lexeme L.float),
      try $ Lang.intLit <$> L.signed sc (lexeme L.decimal),
      Lang.booleanLit <$> lexeme (True <$ symbol "false" <|> False <$ symbol "true"),
      Lang.stringLit <$> lexeme (char '\'' *> manyTill L.charLiteral (char '\''))
    ]

functionCall :: Lang.MonadLang m => Parser (m (Lang.Expr m))
functionCall = do
  (name, arguments) <- call expr
  pure $ Lang.call (Lang.FunctionName name) arguments

expr :: Lang.MonadLang m => Parser (m (Lang.Expr m))
expr =
  let term =
        choice
          [ parens expr,
            try functionCall,
            Lang.lit <$> literal,
            Lang.deref <$> variable
          ]
      operatorTable =
        [ [ prefix "!" (Lang.unop Lang.Not),
            prefix "-" (Lang.unop Lang.Negate),
            prefix "+" id
          ],
          -- 7
          [ infixl_ "*" (Lang.binop Lang.Multiply),
            infixl_ "/" (Lang.binop Lang.Devide)
          ],
          -- 6
          [ infixl_ "+" (Lang.binop Lang.Plus),
            infixl_ "-" (Lang.binop Lang.Minus)
          ],
          [ infix_ "==" (Lang.binop Lang.Equal),
            infix_ ">" (Lang.binop Lang.GreaterThan),
            infix_ "<" (Lang.binop Lang.LessThan),
            infix_ ">=" (Lang.binop Lang.GreaterThanOrEqual),
            infix_ "<=" (Lang.binop Lang.LessThanOrEqual)
          ],
          -- 3
          [ infixr_ "&&" (Lang.binop Lang.And)
          ],
          -- 2
          [ infixr_ "||" (Lang.binop Lang.Or)
          ]
        ]
   in makeExprParser term operatorTable
  where
    infix_ name f = InfixN (f <$ symbol name)
    infixl_ name f = InfixL (f <$ symbol name)
    infixr_ name f = InfixR (f <$ symbol name)
    prefix name f = Prefix (f <$ symbol name)

block :: Lang.MonadLang m => Parser (m (Lang.Stm m))
block = do
  lexeme $ symbol "begin"
  body <- many (statement <* (lexeme . symbol) ";")
  lexeme $ symbol "end"
  pure $ Lang.block body

assignment :: Lang.MonadLang m => Parser (m (Lang.Stm m))
assignment = do
  var <- variable
  lexeme $ symbol ":="
  val <- expr
  pure $ Lang.assign var val

readln :: Lang.MonadLang m => Parser (m (Lang.Stm m))
readln = do
  ("readln", variables) <- call variable
  pure $ Lang.readln variables

writeln :: Lang.MonadLang m => Parser (m (Lang.Stm m))
writeln = do
  ("writeln", arguments) <- call expr
  pure $ Lang.writeln arguments

if_ :: Lang.MonadLang m => Parser (m (Lang.Stm m))
if_ = do
  lexeme $ symbol "if"
  condition <- expr
  lexeme $ symbol "then"
  then_ <- block
  lexeme $ symbol "else"
  else_ <- block
  pure $ Lang.if_ condition then_ else_

while_ :: Lang.MonadLang m => Parser (m (Lang.Stm m))
while_ = do
  lexeme $ symbol "while"
  condition <- expr
  lexeme $ symbol "do"
  body <- block
  pure $ Lang.while_ condition body

statement :: Lang.MonadLang m => Parser (m (Lang.Stm m))
statement =
  choice
    [ try block,
      try writeln,
      try readln,
      try assignment,
      try if_,
      try while_
    ]

type_ :: Parser Lang.Type
type_ =
  choice
    [ Lang.Integer <$ (lexeme . symbol) "integer",
      Lang.Real <$ (lexeme . symbol) "real",
      Lang.Boolean <$ (lexeme . symbol) "boolean",
      Lang.String <$ (lexeme . symbol) "string"
    ]

variablesDeclaration :: Lang.MonadLang m => Parser [(Lang.Variable, Lang.Type, Lang.Lit m)]
variablesDeclaration = do
  lexeme $ symbol "var"
  many $ try do
    var <- variable
    lexeme $ symbol ":"
    varType <- type_
    lexeme $ symbol "="
    varVal <- literal
    lexeme $ symbol ";"
    pure (var, varType, varVal)

function :: Lang.MonadLang m => Parser (Lang.Function m)
function = do
  lexeme $ symbol "function"
  name <- identifier
  arguments <- parens $ argument `sepBy` (lexeme . symbol) ";"
  lexeme $ symbol ":"
  functionType <- type_
  lexeme $ symbol ";"
  variables <- fromMaybe [] <$> optional variablesDeclaration
  body <- block
  lexeme $ symbol ";"
  pure Lang.Function {name = Lang.FunctionName name, type_ = functionType, arguments, variables, body}
  where
    argument = do
      var <- variable
      lexeme $ symbol ":"
      varType <- type_
      pure (var, varType)

module_ :: Lang.MonadLang m => Parser (Lang.Module m)
module_ = do
  variables <- fromMaybe [] <$> optional variablesDeclaration
  functions <- many function
  body <- block
  lexeme $ symbol "."
  eof
  pure Lang.Module {variables, functions, body}

parse :: Lang.MonadLang m => String -> Either String (Lang.Module m)
parse source = case M.parse module_ "" source of
  Left error -> Left $ errorBundlePretty error
  Right module_ -> Right $ module_
