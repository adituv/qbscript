module Compiler.QbScript.Parser.Gen where

import Compiler.QbScript.Parser

import qualified Data.Map.Strict as M
import Data.Word(Word32)
import Numeric(showHex)
import Test.QuickCheck

stripQuotes :: String -> String
stripQuotes ('"':xs@(_:_))
  | last xs == '"' = init xs
stripQuotes xs = xs

escape :: String -> String
escape = stripQuotes . show

substQuote :: String -> String
substQuote = fmap (\x -> M.findWithDefault x x (M.fromList [('\'', '"'), ('"', '\'')]))

newtype Alpha = Alpha Char deriving Show
newtype Digit = Digit Char deriving Show
newtype Ident = Ident String deriving Show
newtype NString = NString String deriving Show
newtype WString = WString String deriving Show
newtype RWord = RWord String deriving Show
newtype Checksum = Checksum Word32

instance Show Checksum where
  show (Checksum x) = reverse . take 8 . reverse $ "0000000" ++ showHex x ""

instance Arbitrary Alpha where
  arbitrary = Alpha <$> elements alpha

instance Arbitrary Digit where
  arbitrary = Digit <$> elements ['0'..'9']

instance Arbitrary Ident where
  arbitrary = Ident <$> ((:) <$> elements alpha <*> listOf (elements ident))

instance Arbitrary NString where
  arbitrary = NString . fmap (toEnum . (`mod` 128) . fromEnum) <$> (arbitrary :: Gen String)

instance Arbitrary WString where
  arbitrary = WString <$> (arbitrary :: Gen String)

instance Arbitrary RWord where
  arbitrary = RWord <$> elements reservedWords

instance Arbitrary Checksum where
  arbitrary = Checksum <$> arbitraryBoundedIntegral
