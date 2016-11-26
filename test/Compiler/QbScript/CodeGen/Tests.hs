module Compiler.QbScript.CodeGen.Tests where

import Compiler.QbScript.AST
import Compiler.QbScript.CodeGen
import Data.GH3.QB

import qualified Data.ByteString as B
import Data.Packer(Packing, runPacking)
import Data.Word(Word8)
import Test.Hspec

testPacking :: Packing () -> [Word8]
testPacking = B.unpack . runPacking 4096

runTests :: Spec
runTests = do
  describe "putInstr" $ do
    it "generates an assignment correctly" $
      testPacking (putInstr (Assign (NonLocal . QbName $ "x") (ELit . SmallLit . LitN $ 2)))
         `shouldBe` [0x01, 0x16, 0x7C, 0xE9, 0x23, 0x73, 0x07, 0x17, 0x02, 0x00, 0x00, 0x00]
    it "generates an if/elseif/else correctly" $
      testPacking (putInstr (IfElse (ELit LitPassthrough, [BareExpr $ ELit LitPassthrough])
                                   [(ELit LitPassthrough, [BareExpr $ ELit LitPassthrough])]
                                    [BareExpr $ ELit LitPassthrough]))
        `shouldBe` [ 0x01, 0x47, 0x06, 0x00, 0x2C, 0x01, 0x2C
                   , 0x01, 0x27, 0x08, 0x00, 0x0D, 0x00, 0x2C, 0x01, 0x2C
                   , 0x01, 0x48, 0x06, 0x00, 0x01, 0x2C, 0x01, 0x28 ]
    it "generates a repeat correctly" $
      testPacking (putInstr (Repeat (ELit . SmallLit . LitN $ 4) [BareExpr $ ELit LitPassthrough]))
        `shouldBe` [ 0x01, 0x20, 0x01, 0x2C, 0x01, 0x21, 0x17, 0x04, 0x00, 0x00, 0x00 ]
    it "generates a switch/case/default correctly" $
      testPacking (putInstr (Switch (ELit . SmallLit . LitN $ 2)
                                   [(LitN 1, [BareExpr $ ELit LitPassthrough])
                                   ,(LitN 2, [BareExpr $ ELit LitPassthrough])]
                                    [BareExpr $ ELit LitPassthrough]))
        `shouldBe` [ 0x01, 0x3C, 0x17, 0x02, 0x00, 0x00, 0x00
                   , 0x01, 0x3E, 0x49, 0x0D, 0x00, 0x17, 0x01, 0x00, 0x00, 0x00, 0x01, 0x2C
                   , 0x01, 0x49, 0x19, 0x00, 0x3E, 0x49, 0x0D, 0x00, 0x17, 0x02, 0x00, 0x00, 0x00, 0x01, 0x2C
                   , 0x01, 0x49, 0x0A, 0x00, 0x3F, 0x49, 0x05, 0x00, 0x01, 0x2C
                   , 0x01, 0x3D ]
    it "generates a break correctly" $
      testPacking (putInstr Break) `shouldBe` [ 0x01, 0x22 ]
    it "generates return correctly" $ do
      testPacking (putInstr $ Return Nothing) `shouldBe` [ 0x01, 0x29 ]
      testPacking (putInstr $ Return (Just (Nothing, ELit . SmallLit . LitN $ 2)))
        `shouldBe` [ 0x01, 0x29, 0x17, 0x02, 0x00, 0x00, 0x00 ]
      testPacking (putInstr $ Return (Just (Just (QbName "x"), ELit . SmallLit . LitN $ 2)))
        `shouldBe` [ 0x01, 0x29, 0x16, 0x7C, 0xE9, 0x23, 0x73, 0x07, 0x17, 0x02, 0x00, 0x00, 0x00 ]
  describe "putSmallLit" $ do
    it "generates an integer correctly" $
      testPacking (putSmallLit (LitN 3)) `shouldBe` [ 0x17, 0x03, 0x00, 0x00, 0x00 ]
    it "generates a hex integer correctly" $
      testPacking (putSmallLit (LitH 0x24)) `shouldBe` [ 0x18, 0x24, 0x00, 0x00, 0x00 ]
    
