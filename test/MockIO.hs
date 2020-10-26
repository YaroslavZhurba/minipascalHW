module MockIO (withMockedIO) where

import Control.Exception (bracket, catch)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hPutStr, openTempFile, stdin, stdout, withFile)

withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content action = bracket prepare clear action
  where
    prepare = do
      dir <- getTemporaryDirectory
      (path, handle) <- openTempFile dir ""
      hPutStr handle content
      hClose handle
      pure path
    clear path = do
      let handler :: IOError -> IO ()
          handler _ = pure ()
      removeFile path `catch` handler

withStdout :: IO () -> IO String
withStdout action = do
  withTempFile "" \path -> do
    withFile path WriteMode \newOut -> do
      bracket
        ( do
            savedOut <- hDuplicate stdout
            newOut `hDuplicateTo` stdout
            pure savedOut
        )
        (`hDuplicateTo` stdout)
        (\_ -> action)
    readFile path

withStdin :: String -> IO () -> IO ()
withStdin content action =
  withTempFile content \path -> do
    withFile path ReadMode \newIn -> do
      bracket
        ( do
            savedIn <- hDuplicate stdin
            newIn `hDuplicateTo` stdin
            pure savedIn
        )
        (`hDuplicateTo` stdin)
        (\_ -> action)

withMockedIO :: String -> IO () -> IO String
withMockedIO input = withStdout . withStdin input
