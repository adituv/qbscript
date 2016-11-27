import qualified Compiler.QbScript.CodeGen.Tests as CodeGen
import qualified Compiler.QbScript.Parser.Tests as Parser

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parser" Parser.runTests
  describe "CodeGen" CodeGen.runTests
