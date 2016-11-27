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
  instrTests
  smallLitTests
  litTests
  dictTests
  arrayTests
  exprTests
  structTests

instrTests :: Spec
instrTests =
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

smallLitTests :: Spec
smallLitTests =
  describe "putSmallLit" $ do
    it "generates an integer correctly" $
      testPacking (putSmallLit (LitN 3)) `shouldBe` [ 0x17, 0x03, 0x00, 0x00, 0x00 ]
    it "generates a hex integer correctly" $
      testPacking (putSmallLit (LitH 0x24)) `shouldBe` [ 0x18, 0x24, 0x00, 0x00, 0x00 ]
    it "generates a key by checksum correctly" $
      testPacking (putSmallLit (LitKey . NonLocal . QbCrc $ 0x78563412)) `shouldBe` [ 0x16, 0x12, 0x34, 0x56, 0x78 ]
    it "generates a key by name correctly" $
      testPacking (putSmallLit (LitKey . NonLocal . QbName $ "x")) `shouldBe` [ 0x16, 0x7C, 0xE9, 0x23, 0x73 ]
    it "generates a local key by checksum correctly" $
      testPacking (putSmallLit (LitKey . Local . QbCrc $ 0x78563412)) `shouldBe` [ 0x2D, 0x16, 0x12, 0x34, 0x56, 0x78 ]
    it "generates a local key by name correctly" $
      testPacking (putSmallLit (LitKey . Local . QbName $ "x")) `shouldBe` [ 0x2D, 0x16, 0x7C, 0xE9, 0x23, 0x73 ]

litTests :: Spec
litTests =
  describe "putLit" $ do
    it "generates a float correctly" $
      testPacking (putLit (LitF 1)) `shouldBe` [ 0x1A, 0x00, 0x00, 0x80, 0x3F ]
    it "generates a vector2 correctly" $
      testPacking (putLit (LitV2 1 1)) `shouldBe` [ 0x1F, 0x00, 0x00, 0x80, 0x3F, 0x00, 0x00, 0x80, 0x3F ]
    it "generates a vector3 correctly" $
      testPacking (putLit (LitV3 1 1 1)) `shouldBe` [ 0x1E, 0x00, 0x00, 0x80, 0x3F
                                                          , 0x00, 0x00, 0x80, 0x3F
                                                          , 0x00, 0x00, 0x80, 0x3F ]
    it "generates a narrow string correctly" $
      testPacking (putLit (LitString "HelloWorld")) `shouldBe` [ 0x1B, 0x0B, 0x00, 0x00, 0x00
                                                               , 0x48, 0x65, 0x6C, 0x6C, 0x6F
                                                               , 0x57, 0x6F, 0x72, 0x6C, 0x64, 0x00 ]
    it "generates a wide string correctly" $
      testPacking (putLit (LitWString "HelloWorld")) `shouldBe`
        [ 0x4C, 0x16, 0x00, 0x00, 0x00
        , 0x00, 0x48, 0x00, 0x65, 0x00, 0x6C, 0x00, 0x6C, 0x00, 0x6F
        , 0x00, 0x57, 0x00, 0x6F, 0x00, 0x72, 0x00, 0x6C, 0x00, 0x64, 0x00, 0x00 ]
    it "generates a struct correctly" $
      testPacking (putLit (LitStruct (Struct []))) `shouldBe`
        [ 0x4A, 0x08, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]

dictTests :: Spec
dictTests =
  describe "putLitDict" $ do
    it "generates an empty passthrough extension correctly" $
      testPacking (putLitDict (ExtendsPT [])) `shouldBe`
        [ 0x03, 0x01, 0x2C, 0x01, 0x04 ]
    it "generates an empty dict correctly" $
      testPacking (putLitDict (Dict [])) `shouldBe` [ 0x03, 0x01, 0x04 ]
    it "generates a passthrough extension correctly" $
      testPacking (putLitDict (ExtendsPT [(QbName "x", ELit LitPassthrough)]))
        `shouldBe` [ 0x03, 0x01, 0x2C, 0x01, 0x16, 0x7C, 0xE9, 0x23, 0x73, 0x07, 0x2C, 0x01, 0x04 ]
    it "generates a dict correctly"  $
      testPacking (putLitDict (Dict [(QbName "x", ELit LitPassthrough)]))
        `shouldBe` [ 0x03, 0x01, 0x16, 0x7C, 0xE9, 0x23, 0x73, 0x07, 0x2C, 0x01, 0x04 ]

arrayTests :: Spec
arrayTests =
  describe "putLitArray" $ do
    it "generates an empty array correctly" $
      testPacking (putLitArray (Array []))
        `shouldBe` [ 0x05, 0x06 ]
    it "generates a 1-element array correctly" $
      testPacking (putLitArray (Array [BareCall (QbName "f") []]))
        `shouldBe` [ 0x05, 0x16, 0x1F, 0xD4, 0x2C, 0x89, 0x06 ]
    it "generates a 2-element array correctly" $
      testPacking (putLitArray (Array [BareCall (QbName "f") [], BareCall (QbName "x") []]))
        `shouldBe` [ 0x05, 0x16, 0x1F, 0xD4, 0x2C, 0x89, 0x09, 0x16, 0x7C, 0xE9, 0x23, 0x73, 0x06 ]

exprTests :: Spec
exprTests =
  describe "putExpr" $ do
    -- TODO
    return ()

structTests :: Spec
structTests =
  describe "putStruct" $ do
    -- TODO
    return ()
