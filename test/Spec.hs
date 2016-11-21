import qualified Compiler.QbScript.Parser.Tests as Parser

import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec $ do
  Parser.runTests
