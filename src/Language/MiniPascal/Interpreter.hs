module Language.MiniPascal.Interpreter (eval) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.IORef
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (for)
import Language.MiniPascal.Lang

data Value
  = IntegerValue Integer
  | RealValue Double
  | BooleanValue Bool
  | StringValue String
  | ResultValue

instance Show Value where
  show = printValue

data Environment = Environment
  { memory :: NonEmpty (Map Variable Value),
    functions :: Map FunctionName (Function Runtime)
  }

newtype Runtime a = Runtime {unRuntime :: ReaderT (IORef Environment) IO a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadFail, MonadReader (IORef Environment), MonadIO)

type instance Lit Runtime = Value

type instance Expr Runtime = Value

type instance Stm Runtime = ()

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { memory = Map.empty :| [],
      functions = Map.empty
    }

readEnvironment :: Runtime Environment
readEnvironment = liftIO . readIORef =<< ask

modifyEnvironment :: (Environment -> Environment) -> Runtime ()
modifyEnvironment update = do
  environmentRef <- ask
  liftIO $ modifyIORef' environmentRef update

withLocalScope :: Runtime a -> Runtime a
withLocalScope action = do
  modifyEnvironment \environment@Environment {memory} ->
    environment {memory = Map.empty <| memory}
  result <- action
  modifyEnvironment \environment@Environment {memory = _ :| memory} ->
    environment {memory = NonEmpty.fromList memory}
  pure result

lookupVariable :: Variable -> Runtime Value
lookupVariable variable = do
  Environment {memory} <- readEnvironment
  pure $ lookup (NonEmpty.toList memory)
  where
    lookup (scope : scopes) =
      case Map.lookup variable scope of
        Just value -> value
        Nothing -> lookup scopes

lookupFunction :: FunctionName -> Runtime (Function Runtime)
lookupFunction functionName = do
  Environment {functions} <- readEnvironment
  pure $ functions Map.! functionName

setVariable :: Variable -> Value -> Runtime ()
setVariable variable value = do
  modifyEnvironment \environment@Environment {memory = memory@(scope :| scopes)} ->
    case alter (NonEmpty.toList memory) of
      Just memory -> environment {memory}
      Nothing -> environment {memory = Map.insert variable value scope :| scopes}
  where
    alter [] = Nothing
    alter (scope : scopes) | variable `Map.member` scope = Just (Map.insert variable value scope :| scopes)
    alter (scope : scopes) = (scope <|) <$> alter scopes

declareVariable :: Variable -> Value -> Runtime ()
declareVariable variable value = do
  modifyEnvironment \environment@Environment {memory = scope :| scopes} ->
    environment {memory = Map.insert variable value scope :| scopes}

declareFunction :: Function Runtime -> Runtime ()
declareFunction function@Function {name} = do
  modifyEnvironment \environment@Environment {functions} ->
    environment {functions = Map.insert name function functions}

applyBinaryOperator :: BinaryOperator -> Value -> Value -> Value
applyBinaryOperator Plus (IntegerValue a) (IntegerValue b) = IntegerValue (a + b)
applyBinaryOperator Plus (RealValue a) (RealValue b) = RealValue (a + b)
applyBinaryOperator Plus (StringValue a) (StringValue b) = StringValue (a <> b)
applyBinaryOperator Minus (IntegerValue a) (IntegerValue b) = IntegerValue (a - b)
applyBinaryOperator Minus (RealValue a) (RealValue b) = RealValue (a - b)
applyBinaryOperator Multiply (IntegerValue a) (IntegerValue b) = IntegerValue (a * b)
applyBinaryOperator Multiply (RealValue a) (RealValue b) = RealValue (a * b)
applyBinaryOperator Devide (RealValue a) (RealValue b) = RealValue (a / b)
applyBinaryOperator And (BooleanValue a) (BooleanValue b) = BooleanValue (a && b)
applyBinaryOperator Or (BooleanValue a) (BooleanValue b) = BooleanValue (a || b)
applyBinaryOperator Equal (IntegerValue a) (IntegerValue b) = BooleanValue (a == b)
applyBinaryOperator Equal (RealValue a) (RealValue b) = BooleanValue (a == b)
applyBinaryOperator Equal (BooleanValue a) (BooleanValue b) = BooleanValue (a == b)
applyBinaryOperator Equal (StringValue a) (StringValue b) = BooleanValue (a == b)
applyBinaryOperator GreaterThan (IntegerValue a) (IntegerValue b) = BooleanValue (a > b)
applyBinaryOperator GreaterThan (RealValue a) (RealValue b) = BooleanValue (a > b)
applyBinaryOperator LessThan (IntegerValue a) (IntegerValue b) = BooleanValue (a < b)
applyBinaryOperator LessThan (RealValue a) (RealValue b) = BooleanValue (a < b)
applyBinaryOperator GreaterThanOrEqual (IntegerValue a) (IntegerValue b) = BooleanValue (a >= b)
applyBinaryOperator GreaterThanOrEqual (RealValue a) (RealValue b) = BooleanValue (a >= b)
applyBinaryOperator LessThanOrEqual (IntegerValue a) (IntegerValue b) = BooleanValue (a <= b)
applyBinaryOperator LessThanOrEqual (RealValue a) (RealValue b) = BooleanValue (a <= b)

applyUnaryOperator :: UnaryOperator -> Value -> Value
applyUnaryOperator Negate (IntegerValue a) = IntegerValue (negate a)
applyUnaryOperator Negate (RealValue a) = RealValue (negate a)
applyUnaryOperator Not (BooleanValue a) = BooleanValue (not a)

printValue :: Value -> String
printValue (IntegerValue x) = show x
printValue (RealValue x) = show x
printValue (BooleanValue True) = "true"
printValue (BooleanValue False) = "false"
printValue (StringValue x) = x

parseValue :: Type -> String -> Value
parseValue Integer string = IntegerValue (read string)
parseValue Real string = RealValue (read string)
parseValue Boolean "true" = BooleanValue True
parseValue Boolean "false" = BooleanValue False
parseValue String string = StringValue string

instance MonadLang Runtime where
  lit = pure
  block stms = sequence_ stms

  intLit = IntegerValue
  realLit = RealValue
  booleanLit = BooleanValue
  stringLit = StringValue

  deref = lookupVariable
  assign variable value = setVariable variable =<< value

  binop op = liftA2 (applyBinaryOperator op)
  unop op = fmap (applyUnaryOperator op)

  call functionName actualArguments = do
    Function {arguments, variables, body} <- lookupFunction functionName
    withLocalScope do
      declareVariable (resultVariable functionName) ResultValue
      for (zip arguments actualArguments) \((name, _), value) ->
        declareVariable name =<< value
      withLocalScope do
        for variables \(name, _, value) -> do
          declareVariable name value
        body
      lookupVariable (resultVariable functionName)

  readln variables = do
    for_ variables \variable@Variable {annotation = Just type_} -> do
      string <- liftIO getLine
      setVariable variable (parseValue type_ string)

  writeln values = do
    values' <- sequence values
    liftIO $ putStrLn $ join $ fmap printValue values'

  if_ cond then_ else_ = do
    cond' <- cond
    case cond' of
      BooleanValue True -> then_
      BooleanValue False -> else_

  while_ cond body = do
    cond' <- cond
    case cond' of
      BooleanValue True -> do
        body
        while_ cond body
      BooleanValue False -> pure ()

runModule :: Module Runtime -> Runtime ()
runModule Module {variables, functions, body} = do
  for variables \(name, _, value) -> do
    setVariable name value
  for functions declareFunction
  body

eval :: Module Runtime -> IO ()
eval module_ = do
  environmentRef <- newIORef emptyEnvironment
  flip runReaderT environmentRef . unRuntime . runModule $ module_
