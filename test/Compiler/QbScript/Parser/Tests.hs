{-# LANGUAGE OverloadedStrings #-}
module Compiler.QbScript.Parser.Tests(runTests) where

import Compiler.QbScript.AST
import Compiler.QbScript.Parser
import Compiler.QbScript.Parser.Gen
import Data.GH3.QB

import Data.Char(toLower)
import Data.String(fromString)
import Numeric
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec

runTests :: Spec
runTests = do
  rwordTests
  identTests
  litTests
  instructionTests
  termTests
  expressionTests
  qbScriptTests

rwordTests :: Spec
rwordTests =
  describe "rword" $ do
    it "correctly parses a word" $ property $
      \x -> parse (rword x) "" `shouldSucceedOn` fromString x
    it "fails to parse a prefix of a word" $ property $
      \x (Ident y) -> parse (rword x) "" `shouldFailOn` fromString (x ++ y)

identTests :: Spec
identTests =
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

litTests :: Spec
litTests =
  describe "lit" $ do
    it "can parse floats" $ property $
      \x -> parse lit "" (fromString $ show x) `shouldParse` LitF x
    it "can parse decimal integers" $ property $
      \x -> parse lit "" (fromString $ show x) `shouldParse` SmallLit (LitN x)
    it "can parse hexadecimal integers" $ property $
      \x -> parse lit "" (fromString $ "0x" ++ showHex x "") `shouldParse` SmallLit (LitH x)
    it "can parse a non-local QB key by name" $ property $
      \(Ident xs) -> fmap toLower xs `notElem` reservedWords
        ==> parse lit "" (fromString xs) `shouldParse` SmallLit (LitKey . NonLocal . QbName $ xs)
    it "can parse a local QB key by name" $ property $
      \(Ident xs) -> fmap toLower xs `notElem` reservedWords
        ==> parse lit "" (fromString $ '%':xs) `shouldParse` SmallLit (LitKey . Local . QbName $ xs)
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
    it "can parse an ascii string" $ property $
      \(NString xs) -> parse lit "" (fromString $ "'" ++ substQuote (escape xs) ++ "'") `shouldParse` LitString (substQuote xs)
    it "fails to parse a string with non-ascii characters" $ property $
      \x (NString xs) -> parse lit "" `shouldFailOn`
                         fromString ("'" ++ toEnum (fromEnum (x :: Char) + 0x80):xs ++ "'")
    it "can parse a wide string" $ property $
      \(WString xs) -> parse lit "" (fromString $ "\"" ++ escape xs ++ "\"") `shouldParse` LitWString xs
    it "can parse a passthrough" $
      parse lit "" "<...>" `shouldParse` LitPassthrough
    -- TODO: maybe quickcheck these?
    it "can parse a passthrough-extended dictionary" $
      parse lit "" "{ <...>,\n test: 1.0,\n $12345678: <...> }"
        `shouldParse` (LitDict . ExtendsPT)
           [(QbName "test", ELit (LitF 1)), (QbCrc 0x12345678, ELit LitPassthrough)]
    it "can parse an unextended dictionary" $
      parse lit "" "{\n asdf: 1.0,\n $deadbeef : 2.0\n }" `shouldParse` (LitDict . Dict)
        [(QbName "asdf", ELit (LitF 1)), (QbCrc 0xdeadbeef, ELit (LitF 2))]
    it "can parse an array of float" $ property $
      \xs -> parse lit "" (fromString $ show (fmap getNonNegative xs :: [Float]))
               `shouldParse` (LitArray . Array . fmap (ELit . LitF)) (fmap getNonNegative xs)
    it "can parse an array over multiple lines" $
      parse lit "" "[\n1.0, 2.0\n]" `shouldParse` (LitArray . Array . fmap (ELit . LitF)) [1,2]
    it "can parse an example struct" $
      parse lit "" "{\n int time = 6;\n qbkey _ = gameframes;\n}" `shouldParse`
        (LitStruct . Struct) [ StructItem QbTInteger (QbName "time") (QbInteger 6)
                             , StructItem QbTKey (QbCrc 0) (QbKey (QbName "gameframes"))]

instructionTests :: Spec
instructionTests =
  describe "instruction" $ do
    it "can parse an assignment by name" $ property $
      \(Ident xs) -> fmap toLower xs `notElem` reservedWords
          ==> parse instruction "" (fromString $ xs ++ " = 1.0") `shouldParse`
             Assign (NonLocal $ QbName xs) (ELit (LitF 1))
    it "can parse an assignment by checksum" $ property $
      \c@(Checksum x) -> parse instruction "" (fromString $ "%$" ++ show c ++ " = 1.0") `shouldParse`
          Assign (Local $ QbCrc x) (ELit (LitF 1))
    it "can parse an if with no else branches" $
      parse instruction "" "if 1.0\n  doSomething()\nendif" `shouldParse`
        IfElse (ELit (LitF 1), [BareExpr $ BareCall (QbName "doSomething") []]) [] []
    it "can parse an if/elseif" $
      parse instruction "" "if 1.0\n  doSomething()\nelseif 2.0\n  doNothing()\nendif"
        `shouldParse` IfElse (ELit (LitF 1), [BareExpr $ BareCall (QbName "doSomething") []])
                             [(ELit (LitF 2), [BareExpr $ BareCall (QbName "doNothing") []])]
                             []
    it "can parse an if/else" $
      parse instruction "" "if 1.0\n  doSomething()\nelse\n  doNothing()\nendif"
        `shouldParse` IfElse (ELit (LitF 1), [BareExpr $ BareCall (QbName "doSomething") []])
                             []
                             [BareExpr $ BareCall (QbName "doNothing") []]
    it "can parse an if/elseif/else" $
      parse instruction "" "if 1.0\n  doSomething()\nelseif 2.0\n  doNothing()\nelse\n  doNothing()\nendif"
        `shouldParse` IfElse (ELit (LitF 1), [BareExpr $ BareCall (QbName "doSomething") []])
                             [(ELit (LitF 2), [BareExpr $ BareCall (QbName "doNothing") []])]
                             [BareExpr $ BareCall (QbName "doNothing") []]
    it "can parse a begin/repeat" $
      parse instruction "" "begin\n  doSomething()\nrepeat (4)" `shouldParse`
        Repeat (ELit . SmallLit . LitN $ 4) [BareExpr $ BareCall (QbName "doSomething") []]
    it "can parse a switch without default" $
      parse instruction "" "switch %i\ncase 1:\n  doSomething()\n  break\nendswitch" `shouldParse`
        Switch (ELit . SmallLit . LitKey . Local . QbName $ "i")
               [(LitN 1, [BareExpr $ BareCall (QbName "doSomething") [], Break] )]
               []
    it "can parse a switch with default" $
      parse instruction "" "switch %i\ncase 1:\n  doSomething()\n  break\ndefault:\n  doNothing()\nendswitch" `shouldParse`
        Switch (ELit . SmallLit . LitKey . Local . QbName $ "i")
               [(LitN 1, [BareExpr $ BareCall (QbName "doSomething") [], Break] )]
               [BareExpr $ BareCall (QbName "doNothing") []]
    it "can parse a break" $
      parse instruction "" "break" `shouldParse` Break
    it "can parse a non-keyword return" $
      parse instruction "" "return 3" `shouldParse` Return (Nothing, ELit (SmallLit (LitN 3)))
    it "can parse a keyword return" $
      parse instruction "" "return (x=2.0)" `shouldParse` Return (Just $ QbName "x", ELit (LitF 2))
    it "can parse a keyword return by crc" $
      parse instruction "" "return ($1234abcd=2.0)" `shouldParse` Return (Just $ QbCrc 0x1234abcd, ELit (LitF 2))
    it "can parse a bare call expression" $
      parse instruction "" "doSomething()"
        `shouldParse` BareExpr (BareCall (QbName "doSomething") [])

termTests :: Spec
termTests =
  describe "term" $ do
    it "can parse an expression in parentheses" $
      parse term "" "(1.0+2.0)" `shouldParse` Paren (Add (ELit (LitF 1)) (ELit (LitF 2)))
    it "can parse a method call with no arguments" $
      parse term "" "a:test()" `shouldParse` MethodCall (NonLocal (QbName "a")) (QbName "test") []
    it "can parse a method call with a value argument" $
      parse term "" "a:test(b)" `shouldParse` MethodCall (NonLocal (QbName "a")) (QbName "test") [(Nothing, eLitKey "b")]
    it "can parse a method call with a keyword argument" $
      parse term "" "a:test(b=c)" `shouldParse` MethodCall (NonLocal (QbName "a")) (QbName "test") [(Just $ QbName "b", eLitKey "c")]
    it "can parse a method call with multiple arguments" $
      parse term "" "a:test(b,c=2.0,3.0)" `shouldParse`
        MethodCall (NonLocal (QbName "a")) (QbName "test")
          [ (Nothing, eLitKey "b")
          , (Just $ QbName "c", ELit . LitF $ 2)
          , (Nothing, ELit . LitF $ 3)
          ]
    it "can parse a function call with no arguments" $
      parse term "" "test()" `shouldParse` BareCall (QbName "test") []
    it "can parse a function call with a value argument" $
      parse term "" "test(b)" `shouldParse` BareCall (QbName "test")
        [(Nothing, eLitKey "b")]
    it "can parse a function call with a keyword argument" $
      parse term "" "test(b=c)" `shouldParse` BareCall (QbName "test")
        [(Just $ QbName "b", eLitKey "c")]
    it "can parse a function call with multiple arguments" $
      parse term "" "test(b,c=2.0,3.0)" `shouldParse`
        BareCall (QbName "test")
          [ (Nothing, eLitKey "b")
          , (Just $ QbName "c", ELit . LitF $ 2)
          , (Nothing, ELit . LitF $ 3)
          ]
    it "can parse a literal on its own" $
      parse term "" "[ 1 ]" `shouldParse` (ELit . LitArray . Array $ [ELit . SmallLit . LitN $ 1])

expressionTests :: Spec
expressionTests =
  describe "expr" $ do
    it "can parse a bare term" $
      parse expr "" "test(b)" `shouldParse` BareCall (QbName "test")
        [(Nothing, eLitKey "b")]
    it "can parse any single operator expression" $ do
      parse expr "" "*a" `shouldParse` Deref (eLitKey "a")
      parse expr "" "a.b" `shouldParse` Member (eLitKey "a") (QbName "b")
      parse expr "" "a[b]" `shouldParse` Index (eLitKey "a") (eLitKey "b")
      parse expr "" "!a" `shouldParse` Not (eLitKey "a")
      parse expr "" "-a" `shouldParse` Neg (eLitKey "a")
      parse expr "" "a*b" `shouldParse` Mul (eLitKey "a") (eLitKey "b")
      parse expr "" "a/b" `shouldParse` Div (eLitKey "a") (eLitKey "b")
      parse expr "" "a+b" `shouldParse` Add (eLitKey "a") (eLitKey "b")
      parse expr "" "a-b" `shouldParse` Sub (eLitKey "a") (eLitKey "b")
      parse expr "" "a<b" `shouldParse` Lt (eLitKey "a") (eLitKey "b")
      parse expr "" "a>b" `shouldParse` Gt (eLitKey "a") (eLitKey "b")
      parse expr "" "a<=b" `shouldParse` Lte (eLitKey "a") (eLitKey "b")
      parse expr "" "a>=b" `shouldParse` Gte (eLitKey "a") (eLitKey "b")
      parse expr "" "a==b" `shouldParse` Eq (eLitKey "a") (eLitKey "b")
      parse expr "" "a!=b" `shouldParse` Neq (eLitKey "a") (eLitKey "b")
      parse expr "" "a^b" `shouldParse` Xor (eLitKey "a") (eLitKey "b")
      parse expr "" "a&&b" `shouldParse` And (eLitKey "a") (eLitKey "b")
      parse expr "" "a||b" `shouldParse` Or (eLitKey "a") (eLitKey "b")
    it "handles precedence of combinations of operators correctly" $ do
      parse expr "" "*a.b" `shouldParse` Member (Deref $ eLitKey "a") (QbName "b")
      parse expr "" "!a&&b" `shouldParse` And (Not $ eLitKey "a") (eLitKey "b")
      parse expr "" "-3*4" `shouldParse` Mul (Neg . ELit . SmallLit . LitN $ 3)
                                             (ELit . SmallLit . LitN $ 4)
      parse expr "" "a*b+c" `shouldParse` Add (Mul (eLitKey "a") (eLitKey "b")) (eLitKey "c")
      parse expr "" "a*b < c" `shouldParse` Lt (Mul (eLitKey "a") (eLitKey "b")) (eLitKey "c")
      parse expr "" "a<b == b<c" `shouldParse` Eq (Lt (eLitKey "a") (eLitKey "b"))
                                                  (Lt (eLitKey "b") (eLitKey "c"))
      parse expr "" "a&&b||c" `shouldParse` Or (And (eLitKey "a") (eLitKey "b")) (eLitKey "c")

eLitKey :: String -> Expr
eLitKey = ELit . SmallLit . LitKey . NonLocal . QbName

qbScriptTests :: Spec
qbScriptTests =
  describe "qbScript" $ do
    -- TODO
    return ()
