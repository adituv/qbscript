module Main where

import Compiler.QbScript.CodeGen
import Compiler.QbScript.Parser

import qualified Data.ByteString as B
import qualified Data.Text.Lazy.IO as TIO
import Text.Megaparsec(runParser, parseErrorPretty)
import System.Environment(getProgName, getArgs)
import System.IO(IOMode(..), withFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, dst] -> compileScript src dst
    (src:dst:_) -> do
      putStrLn "WARNING: only two command line arguments taken.  All others ignored"
      compileScript src dst
    _ -> do
      putStrLn "ERROR: no command line arguments given."
      putStr "Usage: "
      getProgName >>= putStr
      putStrLn " source dest"

compileScript :: FilePath -> FilePath -> IO ()
compileScript src dst =
    withFile dst WriteMode $ \wh ->
      withFile src ReadMode $ \rh ->
        do
          source <- TIO.hGetContents rh
          let result = runParser qbScript src source
          case result of
            Left err -> fail $ parseErrorPretty err
            _ -> pure ()

          let Right ast = result
          let code = genScriptCode ast
          B.hPut wh code
