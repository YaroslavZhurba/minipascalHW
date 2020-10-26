module Language.MiniPascal.TypeChecker (check) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State (MonadState, State, evalState, gets, modify')
import Data.Functor.Identity
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (for)
import Language.MiniPascal.AST
import Language.MiniPascal.Lang

-- Type checker EDSL --

data Environment = Environment
  { variableScopes :: NonEmpty (Map Variable Type),
    functions :: Map FunctionName (Function Identity)
  }

newtype TypeCheck a = TypeCheck {unTypeCheck :: ExceptT String (State Environment) a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadError String, MonadState Environment)

runTypeCheck :: TypeCheck a -> Either String a
runTypeCheck = flip evalState emptyEnvironment . runExceptT . unTypeCheck

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { variableScopes = Map.empty :| [],
      functions = Map.empty
    }

withLocalScope :: TypeCheck a -> TypeCheck a
withLocalScope action = do
  -- save scope
  savedVariableScopes <- gets variableScopes
  -- add local scope
  modify' \environment -> environment {variableScopes = Map.empty <| savedVariableScopes}
  -- run action
  result <- action
  -- restore scope
  modify' \environment -> environment {variableScopes = savedVariableScopes}
  pure result

declareVariable :: Variable -> Type -> TypeCheck ()
declareVariable variable type_ = do
  isDuplicate <- gets \Environment {variableScopes = scope :| _} -> variable `Map.member` scope
  when isDuplicate do
    typeCheckError $ "Duplicated variable " <> show variable
  modify' \Environment {functions, variableScopes = scope :| rest} ->
    Environment {functions, variableScopes = Map.insert variable type_ scope :| rest}

declareFunction :: Function Identity -> TypeCheck ()
declareFunction function@Function {name} = do
  isDuplicate <- gets \Environment {functions} -> name `Map.member` functions
  when isDuplicate do
    typeCheckError $ "Duplicated function " <> show name
  modify' \Environment {functions, variableScopes} ->
    Environment {variableScopes, functions = Map.insert name function functions}

lookupVariable :: Variable -> TypeCheck Type
lookupVariable variable = do
  scopes <- gets (NonEmpty.toList . variableScopes)
  lookup scopes
  where
    lookup [] = typeCheckError $ "No variable " <> show variable <> " in scope"
    lookup (scope : scopes) =
      case Map.lookup variable scope of
        Just type_ -> pure type_
        Nothing -> lookup scopes

lookupFunction :: FunctionName -> TypeCheck (Function Identity)
lookupFunction functionName = do
  maybeFunction <- gets \Environment {functions} -> Map.lookup functionName functions
  case maybeFunction of
    Just function -> pure function
    Nothing -> typeCheckError $ "No function " <> show functionName <> " declared"

typeCheckError :: String -> TypeCheck a
typeCheckError = throwError

-- Type checker --

inferLiteralType :: Literal -> Type
inferLiteralType IntegerLiteral {} = Integer
inferLiteralType RealLiteral {} = Real
inferLiteralType BooleanLiteral {} = Boolean
inferLiteralType StringLiteral {} = String

checkVariableDeclaration :: (Variable, Type, Literal) -> TypeCheck ()
checkVariableDeclaration (name, type_, value) = do
  let valueType = inferLiteralType value
  unless (valueType == type_) do
    typeCheckError $ "Variable " <> show name <> " should be initialized with " <> show type_ <> " literal, but value has " <> show valueType <> " type"

binaryOperatorInputs :: BinaryOperator -> [Type]
binaryOperatorInputs op = case op of
  Plus -> numeric <> [String]
  Minus -> numeric
  Multiply -> numeric
  Devide -> [Real]
  And -> [Boolean]
  Or -> [Boolean]
  Equal -> all
  GreaterThan -> numeric
  LessThan -> numeric
  GreaterThanOrEqual -> numeric
  LessThanOrEqual -> numeric
  where
    all = [Integer, Real, String, Boolean]
    numeric = [Integer, Real]

binaryOperatorOutput :: BinaryOperator -> Type -> Type
binaryOperatorOutput op a = case op of
  Plus -> polymorphic
  Minus -> polymorphic
  Multiply -> polymorphic
  Devide -> Real
  And -> Boolean
  Or -> Boolean
  Equal -> Boolean
  GreaterThan -> Boolean
  LessThan -> Boolean
  GreaterThanOrEqual -> Boolean
  LessThanOrEqual -> Boolean
  where
    polymorphic = a

unaryOperatorInputs :: UnaryOperator -> [Type]
unaryOperatorInputs op = case op of
  Negate -> [Integer, Real]
  Not -> [Boolean]

unaryOperatorOutput :: UnaryOperator -> Type -> Type
unaryOperatorOutput op a = case op of
  Negate -> polymorphic
  Not -> Boolean
  where
    polymorphic = a

inferExpressionType :: Expression -> TypeCheck Type
inferExpressionType (Literal literal) = pure $ inferLiteralType literal
inferExpressionType (Dereference variable) = lookupVariable variable
inferExpressionType (BinaryOperator op a b) = do
  aType <- inferExpressionType a
  bType <- inferExpressionType b
  unless (aType == bType) do
    typeCheckError $ "Binary operator (" <> show op <> ") operands should have the same type"
  let validTypes = binaryOperatorInputs op
  unless (aType `elem` validTypes) do
    typeCheckError $ "Binary operator (" <> show op <> ") can be applied only to " <> intercalate ", " (show <$> validTypes)
  pure $ binaryOperatorOutput op aType
inferExpressionType (UnaryOperator op x) = do
  xType <- inferExpressionType x
  let validTypes = unaryOperatorInputs op
  unless (xType `elem` validTypes) do
    typeCheckError $ "Unary operator (" <> show op <> ") can only be applied " <> intercalate ", " (show <$> validTypes)
  pure $ unaryOperatorOutput op xType
inferExpressionType (Call functionName actualParameters) = do
  Function {arguments, type_} <- lookupFunction functionName
  actualParametersTypes <- for actualParameters inferExpressionType
  let formalParametersTypes = snd <$> arguments
  unless (actualParametersTypes == formalParametersTypes) do
    typeCheckError $ "Invalid arguments for " <> show functionName
  pure type_

refineStatement :: Statement -> TypeCheck Statement
refineStatement (Block statements) = Block <$> for statements refineStatement
refineStatement statement@(Assignment variable value) = do
  variableType <- lookupVariable variable
  valueType <- inferExpressionType value
  unless (variableType == valueType) do
    typeCheckError $ "Variable " <> show variable <> " is " <> show variableType <> ", but value of type " <> show valueType <> " is assigned to it"
  pure statement
refineStatement (ReadLn variables) = do
  inferedVariables <- for variables \variable -> do
    type_ <- lookupVariable variable
    pure variable {annotation = Just type_}
  pure $ ReadLn inferedVariables
refineStatement statement@(WriteLn values) = do
  for values inferExpressionType
  pure statement
refineStatement (If condition then_ else_) = do
  conditionType <- inferExpressionType condition
  unless (conditionType == Boolean) do
    typeCheckError $ "'if' condition should be boolean instead of " <> show conditionType
  refinedThan <- refineStatement then_
  refinedElse <- refineStatement else_
  pure $ If condition refinedThan refinedElse
refineStatement (While condition body) = do
  conditionType <- inferExpressionType condition
  unless (conditionType == Boolean) do
    typeCheckError $ "'while' condition should be boolean instead of " <> show conditionType
  refinedBody <- refineStatement body
  pure $ While condition refinedBody

refineFunction :: Function Identity -> TypeCheck (Function Identity)
refineFunction function@Function {name, arguments, variables, body, type_} = do
  withLocalScope do
    declareVariable (resultVariable name) type_
    for arguments \(variable, type_) -> do
      declareVariable variable type_
    withLocalScope do
      for variables \variable@(name, type_, _) -> do
        checkVariableDeclaration variable
        declareVariable name type_
      refinedBody <- refineStatement $ runIdentity body
      case refinedBody of
        Block [] -> do
          typeCheckError $ "Function " <> show name <> " can not be empty"
        Block statements -> do
          let returnStatement = last statements
          case returnStatement of
            Assignment (Variable var _) value | FunctionName var == name -> do
              returnType <- inferExpressionType value
              unless (returnType == type_) do
                typeCheckError $ "Function " <> show name <> " should return " <> show type_ <> ", but type of return value is " <> show returnType
            _ -> do
              typeCheckError $ "Function " <> show name <> " should be ended with result"
        _ -> typeCheckError $ "Function (" <> show name <> ") body should starts from 'begin'"
      pure (function :: Function Identity) {body = pure refinedBody}

refineModule :: Module Identity -> TypeCheck (Module Identity)
refineModule module_@Module {variables, functions, body} = do
  for variables \variable@(name, type_, _) -> do
    checkVariableDeclaration variable
    declareVariable name type_
  refinedFunctions <- for functions \function -> do
    refinedFunction <- refineFunction function
    declareFunction function
    pure refinedFunction
  refinedBody <- refineStatement $ runIdentity body
  pure module_ {functions = refinedFunctions, body = pure refinedBody}

-----------------

check :: Module Identity -> Either String (Module Identity)
check module_ = runTypeCheck (refineModule module_)
