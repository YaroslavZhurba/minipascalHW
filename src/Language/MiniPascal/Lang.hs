module Language.MiniPascal.Lang where

import Data.Function (on)
import Data.String (IsString (..))

data Type = Integer | Real | Boolean | String
  deriving stock (Eq)

newtype FunctionName = FunctionName String
  deriving newtype (Eq, Ord, IsString)

data Variable = Variable {name :: String, annotation :: Maybe Type}

instance Eq Variable where
  (==) = (==) `on` (name :: Variable -> String)

instance Ord Variable where
  compare = compare `on` (name :: Variable -> String)

instance IsString Variable where
  fromString name = Variable {name, annotation = Nothing}

data BinaryOperator
  = -- numeric and string
    Plus
  | -- numeric
    Minus
  | Multiply
  | Devide
  | -- boolean
    And
  | Or
  | -- compare
    Equal
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual
  deriving stock (Eq)

data UnaryOperator
  = Not
  | Negate
  deriving stock (Eq)

data Function m = Function
  { name :: FunctionName,
    arguments :: [(Variable, Type)],
    variables :: [(Variable, Type, Lit m)],
    type_ :: Type,
    body :: m (Stm m)
  }

data Module m = Module
  { variables :: [(Variable, Type, Lit m)],
    functions :: [Function m],
    body :: m (Stm m)
  }

type family Lit (m :: * -> *) = lit | lit -> m

type family Expr (m :: * -> *) = expr | expr -> m

type family Stm (m :: * -> *) = stm | stm -> m

class Monad m => MonadLang m where
  intLit :: Integer -> (Lit m)
  realLit :: Double -> (Lit m)
  booleanLit :: Bool -> (Lit m)
  stringLit :: String -> (Lit m)

  lit :: Lit m -> m (Expr m)

  block :: [m (Stm m)] -> m (Stm m)

  deref :: Variable -> m (Expr m)
  binop :: BinaryOperator -> m (Expr m) -> m (Expr m) -> m (Expr m)
  unop :: UnaryOperator -> m (Expr m) -> m (Expr m)
  call :: FunctionName -> [m (Expr m)] -> m (Expr m)
  assign :: Variable -> m (Expr m) -> m (Stm m)
  readln :: [Variable] -> m (Stm m)
  writeln :: [m (Expr m)] -> m (Stm m)
  if_ :: m (Expr m) -> m (Stm m) -> m (Stm m) -> m (Stm m)
  while_ :: m (Expr m) -> m (Stm m) -> m (Stm m)

int :: MonadLang m => Integer -> m (Expr m)
int = lit . intLit

real :: MonadLang m => Double -> m (Expr m)
real = lit . realLit

boolean :: MonadLang m => Bool -> m (Expr m)
boolean = lit . booleanLit

string :: MonadLang m => String -> m (Expr m)
string = lit . stringLit

(.>) :: MonadLang m => m (Expr m) -> m (Expr m) -> m (Expr m)
(.>) = binop GreaterThan

(.+), (.-), (.*) :: MonadLang m => m (Expr m) -> m (Expr m) -> m (Expr m)
(.+) = binop Plus
(.-) = binop Minus
(.*) = binop Multiply

infixl 6 .+, .-

infixl 7 .*

infix 4 .>

(.=) :: MonadLang m => Variable -> m (Expr m) -> m (Stm m)
(.=) = assign

infix 0 .=

instance Show Type where
  show Integer = "integer"
  show Real = "real"
  show Boolean = "boolean"
  show String = "string"

resultVariable :: FunctionName -> Variable
resultVariable (FunctionName name) = Variable name Nothing

instance Show FunctionName where
  show (FunctionName x) = x

instance Show Variable where
  show (Variable x Nothing) = x
  show (Variable x (Just annotation)) = x <> " {" <> show annotation <> "}"

instance Show BinaryOperator where
  show Plus = "+"
  show Minus = "-"
  show Multiply = "*"
  show Devide = "/"
  show And = "&&"
  show Or = "||"
  show Equal = "="
  show GreaterThan = ">"
  show LessThan = "<"
  show GreaterThanOrEqual = ">="
  show LessThanOrEqual = "<="

instance Show UnaryOperator where
  show Not = "!"
  show Negate = "-"
