module Main where

import Language.MiniPascal
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["help"] -> help
    ["pprint", path] -> pprint path
    ["format", path] -> format path
    ["check", path] -> check_ path
    ["run", path] -> run path
    _ -> invalidCommand

help :: IO ()
help = do
  putStrLn "minipascal help -- print help"
  putStrLn "minipascal pprint %path -- print formatted file"
  putStrLn "minipascal format %path -- format file"
  putStrLn "minipascal check %path -- validate and typecheck file"
  putStrLn "minipascal run %path -- run file"

invalidCommand :: IO ()
invalidCommand = do
  putStrLn "Invalid command, use these commands:"
  help

pprint :: String -> IO ()
pprint path = do
  module_ <- readModule path
  putStrLn $ prettyPrint module_

format :: String -> IO ()
format path = do
  module_ <- readModule path
  writeFile path (prettyPrint module_)

check_ :: String -> IO ()
check_ path = do
  module_ <- readModule path
  case check module_ of
    Left error -> putStrLn error
    Right _ -> putStrLn "No errors"

run :: String -> IO ()
run path = do
  module_ <- readModule path
  case check module_ of
    Left error -> putStrLn error
    Right module_ -> eval (fromAST module_)

readModule :: MonadLang m => FilePath -> IO (Module m)
readModule path = do
  source <- readFile path
  case parse source of
    Left error -> do
      putStrLn error
      exitFailure
    Right module_ -> pure module_
