module Main where

import Compiler.QbScript.CodeGen
import Compiler.QbScript.Parser

import qualified Data.ByteString as B
import qualified Data.Text.Lazy.IO as TIO
import System.Environment(getArgs, getProgName)
import System.IO(IOMode(..), withBinaryFile, withFile)
import Text.Megaparsec(runParser, parseErrorPretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (src:dst:xs) -> do
      case xs of
        [] -> pure ()
        _  -> putStrLn "WARNING: only two command line arguments taken.  All others ignored"
      compileScript src dst
    _ -> do
      putStrLn "ERROR: too few command line arguments given."
      putStr "Usage: "
      getProgName >>= putStr
      putStrLn " source dest"

compileScript :: FilePath -> FilePath -> IO ()
compileScript src dst = do
  withFile dst WriteMode $ \wh ->
    withFile src ReadMode $ \rh -> do
      source <- TIO.hGetContents rh
      let result = runParser qbScript src source
      ast <- case result of
        Left err -> fail $ parseErrorPretty err
        Right ast -> pure ast
      let code = genScriptCode ast
      B.hPut wh code

