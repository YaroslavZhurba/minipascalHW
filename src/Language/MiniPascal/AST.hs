module Language.MiniPascal.AST
  ( Statement (..),
    Literal (..),
    Expression (..),
    toAST,
    fromAST,
  )
where

import Control.Applicative (liftA2, liftA3)
import Data.Functor.Identity
import Data.Traversable (for)
import Language.MiniPascal.Lang

data Statement
  = Block [Statement]
  | Assignment Variable Expression
  | ReadLn [Variable]
  | WriteLn [Expression]
  | If Expression Statement Statement
  | While Expression Statement
  deriving stock (Show, Eq)

data Literal
  = IntegerLiteral Integer
  | RealLiteral Double
  | BooleanLiteral Bool
  | StringLiteral String
  deriving stock (Show, Eq)

data Expression
  = Literal Literal
  | Dereference Variable
  | BinaryOperator BinaryOperator Expression Expression
  | UnaryOperator UnaryOperator Expression
  | Call FunctionName [Expression]
  deriving stock (Show, Eq)

type instance Lit Identity = Literal

type instance Expr Identity = Expression

type instance Stm Identity = Statement

instance MonadLang Identity where
  lit = pure . Literal
  block stms = Block <$> sequence stms

  intLit = IntegerLiteral
  realLit = RealLiteral
  booleanLit = BooleanLiteral
  stringLit = StringLiteral

  deref = pure . Dereference
  binop op = liftA2 (BinaryOperator op)
  unop op = fmap (UnaryOperator op)
  call functionName arguments = Call functionName <$> sequence arguments
  assign variable = fmap (Assignment variable)
  readln arguments = pure $ ReadLn arguments
  writeln arguments = WriteLn <$> sequence arguments

  if_ = liftA3 If
  while_ = liftA2 While

deriving instance Show (Function Identity)

deriving instance Show (Module Identity)

deriving instance Eq (Function Identity)

deriving instance Eq (Module Identity)

-- Interpret as AST

toAST :: Module Identity -> Module Identity
toAST = id

-- Parse from AST

fromLiteral :: MonadLang m => Literal -> Lit m
fromLiteral (IntegerLiteral x) = intLit x
fromLiteral (RealLiteral x) = realLit x
fromLiteral (BooleanLiteral x) = booleanLit x
fromLiteral (StringLiteral x) = stringLit x

fromExpression :: MonadLang m => Expression -> m (Expr m)
fromExpression (Literal x) = lit (fromLiteral x)
fromExpression (Dereference var) = deref var
fromExpression (BinaryOperator op a b) = binop op (fromExpression a) (fromExpression b)
fromExpression (UnaryOperator op a) = unop op (fromExpression a)
fromExpression (Call function arguments) = call function (fromExpression <$> arguments)

fromStatement :: MonadLang m => Statement -> m (Stm m)
fromStatement (Block statements) = block (fromStatement <$> statements)
fromStatement (Assignment var val) = assign var (fromExpression val)
fromStatement (ReadLn variables) = readln variables
fromStatement (WriteLn values) = writeln (fromExpression <$> values)
fromStatement (If condition then_ else_) =
  (if_ (fromExpression condition))
    do fromStatement then_
    do fromStatement else_
fromStatement (While condition body) =
  while_ (fromExpression condition) do
    fromStatement body

fromVariable :: MonadLang m => (Variable, Type, Literal) -> (Variable, Type, Lit m)
fromVariable (name, type_, value) = (name, type_, fromLiteral value)

fromFunction :: MonadLang m => Function Identity -> Function m
fromFunction Function {name, arguments, variables, type_, body} =
  Function
    { name,
      type_,
      arguments,
      variables = fromVariable <$> variables,
      body = fromStatement $ runIdentity body
    }

fromModule :: MonadLang m => Module Identity -> Module m
fromModule Module {variables, functions, body} =
  Module
    { variables = fromVariable <$> variables,
      functions = fromFunction <$> functions,
      body = fromStatement $ runIdentity body
    }

fromAST :: MonadLang m => Module Identity -> Module m
fromAST = fromModule
