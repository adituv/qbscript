{-# LANGUAGE OverloadedStrings #-}
module Compiler.QbScript.Parser.Tests(runTests) where

import Compiler.QbScript.AST
import Compiler.QbScript.Parser
import Data.GH3.QB;

import Data.String(fromString)
import Data.Word(Word32)
import Numeric
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec

newtype Alpha = Alpha Char deriving Show
newtype Digit = Digit Char deriving Show
newtype Ident = Ident String deriving Show
newtype RWord = RWord String deriving Show
newtype Checksum = Checksum Word32

instance Show Checksum where
  show (Checksum x) = reverse . take 8 . reverse $ "0000000" ++ showHex x ""

instance Arbitrary Alpha where
  arbitrary = Alpha <$> elements alpha

instance Arbitrary Digit where
  arbitrary = Digit <$> elements ['0'..'9']

instance Arbitrary Ident where
  arbitrary = Ident <$> listOf1 (elements ident)

instance Arbitrary RWord where
  arbitrary = RWord <$> elements reservedWords

instance Arbitrary Checksum where
  arbitrary = Checksum . read <$> vectorOf 8 (elements ['0'..'9'])

runTests :: Spec
runTests = do -- TODO: Quickcheck as much as possible
  describe "rword" $ do
    it "correctly parses a word" $ property $
      \x -> parse (rword x) "" `shouldSucceedOn` fromString x
    it "fails to parse a prefix of a word" $ property $
      \x (Ident y) -> parse (rword x) "" `shouldFailOn` fromString (x ++ y)
  describe "identifier" $ do
    it "correctly parses a valid identifier" $ property $
      \(Alpha x) (Ident xs) -> (x : xs) `notElem` reservedWords
                   ==> parse identifier "" (fromString $ x : xs) `shouldParse` (x : xs)
    it "fails to parse an identifier starting with a digit" $ property $
      \(Digit x) xs -> parse identifier "" `shouldFailOn` fromString (x:xs)
    it "fails to parse an identifier starting with an underscore" $ property $
      \xs -> parse identifier "" `shouldFailOn` fromString ('_':xs)
    it "fails to parse a reserved word" $ property $
      \(RWord xs) -> parse identifier "" `shouldFailOn` fromString xs
  describe "lit" $ do
    it "can parse floats" $ property $
      \x -> parse lit "" (fromString $ show x) `shouldParse` LitF x
    it "can parse decimal integers" $ property $
      \x -> parse lit "" (fromString $ show x) `shouldParse` SmallLit (LitN x)
    it "can parse hexadecimal integers" $ property $
      \x -> parse lit "" (fromString $ "0x" ++ showHex x "") `shouldParse` SmallLit (LitH x)
    it "can parse a non-local QB key by name" $ property $
      \(Alpha x) (Ident xs) -> xs `notElem` reservedWords
        ==> parse lit "" (fromString $ x:xs) `shouldParse` SmallLit (LitKey . NonLocal . QbName $ x:xs)
    it "can parse a local QB key by name" $ property $
      \(Alpha x) (Ident xs) -> (x:xs) `notElem` reservedWords
        ==> parse lit "" (fromString $ '%':x:xs) `shouldParse` SmallLit (LitKey . Local . QbName $ x:xs)
    it "can parse a non-local QB key by checksum" $ property $
      \c@(Checksum x) -> parse lit "" (fromString $ '$':show c)
        `shouldParse` SmallLit (LitKey . NonLocal . QbCrc $ x)
    it "can parse a local QB key by checksum" $ property $
      \c@(Checksum x) -> parse lit "" (fromString $ '%':'$':show c)
        `shouldParse` SmallLit (LitKey . Local . QbCrc $ x)
    it "can parse a vector2" $ property $
      \x y -> parse lit "" (fromString $ "(" ++ show x ++ "," ++ show y ++ ")")
                `shouldParse` LitV2 x y
    it "can parse a vector3" $ property $
      \x y z -> parse lit "" (fromString $ "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")")
                  `shouldParse` LitV3 x y z


