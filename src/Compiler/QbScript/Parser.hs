module Compiler.QbScript.Parser where

import           Prelude hiding(repeat)

import           Compiler.QbScript.AST
import           Data.GH3.QB(QbKey(..), Struct, canonicalise)

import           Control.Monad(void)
import           Data.Char(toLower)
import           Data.Map(Map)
import qualified Data.Map.Strict as M
import           Data.Scientific(toRealFloat)
import           Data.Word(Word32)
import           Text.Megaparsec hiding(newline)
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text(Parser)

-- * Lexer

identChar :: Parser Char
identChar = oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"

spaceChar' :: Parser ()
spaceChar' = void (char ' ' <|> char '\t')

opChar :: Parser Char
opChar = oneOf ".:+-*/=<>&|^"

spaceConsumer :: Parser ()
spaceConsumer = L.space spaceChar'
                        (L.skipLineComment "//")
                        (L.skipBlockComment "/*" "*/")

blockKeywords :: [String]
blockKeywords = [ "if", "else", "elseif", "endif", "begin", "repeat", "script", "endscript"
                , "switch", "case", "default", "endswitch" ]

blockKeyword :: Parser String
blockKeyword = choice $ fmap (try . symbol) blockKeywords

reservedWords :: [String]
reservedWords = blockKeywords ++ [ "break", "random", "random2"
                , "randomrange", "randomrange2", "randompermute", "randomshuffle", "not"
                , "useheap" ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol' spaceConsumer

op :: String -> Parser String
op n = (lexeme . try) (string n <* notFollowedBy opChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

newline :: Parser ()
newline = void $ some (lexeme eol)

comma :: Parser ()
comma = void $ symbol ","

colon :: Parser ()
colon = void $ symbol ":"

equals :: Parser ()
equals = void $ symbol "="

signed :: Num a => Parser a -> Parser a
signed = L.signed spaceConsumer

decimal :: Parser Integer
decimal = lexeme (signed L.decimal)

hexadecimal :: Parser Integer
hexadecimal = lexeme (char '0' *> char' 'x' *> L.hexadecimal)

checksum :: Parser Word32
checksum = lexeme $ read . ("0x"++) <$> (char '$' *> count 8 hexDigitChar)

float :: Parser Float
float = toRealFloat <$> lexeme (signed L.scientific)

rword :: String -> Parser ()
rword w = try $ string' w *> notFollowedBy identChar *> spaceConsumer

identifier :: Parser String
identifier = (lexeme . try) (some identChar >>= check)
  where
    check x = if fmap toLower x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

narrowString :: Parser String
narrowString = lexeme $ char '\'' *> manyTill (L.charLiteral >>= check) (char '\'')
  where
    check x = if fromEnum x < 256
                then return x
                else fail $ "character " ++ show x ++ " out of range in narrow string"

wideString :: Parser String
wideString = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

passthrough :: Parser ()
passthrough = void (symbol "<...>")

-- * Parser

-- * Literals

lit :: Parser Lit
lit = choice $ fmap try
        [ LitF <$> float
        , SmallLit <$> smallLit
        , LitString <$> narrowString
        , LitWString <$> wideString
        , LitDict <$> dict
        , LitArray <$> array
        , LitStruct <$> struct
        , LitPassthrough <$ passthrough
        ]

smallLit :: Parser SmallLit
smallLit = choice $ fmap try
             [ LitN . fromInteger <$> decimal
             , LitH . fromInteger <$> hexadecimal
             , LitKey <$> name
             ]

name :: Parser Name
name =  Local <$> (symbol "%" *> qbKey)
    <|> NonLocal <$> qbKey

qbKey :: Parser QbKey
qbKey =  QbCrc <$> checksum
     <|> QbName <$> identifier

dict :: Parser Dict
dict = braces $ ExtendsPT <$> (passthrough *> comma *> dict')
             <|> Dict <$> dict'
  where
    dict' :: Parser (Map QbKey Expr)
    dict' = M.fromList <$> entry `sepBy` comma

    entry :: Parser (QbKey, Expr)
    entry = (,) <$> (canonicalise <$> qbKey) <*> (colon *> expr)

array :: Parser Array
array = Array <$> brackets (expr `sepBy` comma)

-- ** Structs

struct :: Parser Struct
struct = fail "TODO"

-- * Instructions

lineTerm :: Parser ()
lineTerm = newline <|> eof

instructions :: Parser [Instruction]
instructions = many instruction

instruction :: Parser Instruction
instruction = choice (fmap try
  [ Assign <$> name <*> (equals *> expr)
  , ifelse
  , repeat
  , switch
  , BareExpr <$> expr
  ]) <* lineTerm

ifelse :: Parser Instruction
ifelse = IfElse <$> ((:) <$> if' <*> many elseif)
                <*> else'
                <*  rword "endif"

if' :: Parser (Expr, [Instruction])
if' = (,) <$> (rword "if" *> expr <* newline)
          <*> instructions

elseif :: Parser (Expr, [Instruction])
elseif = (,) <$> (rword "elseif" *> expr <* newline)
             <*> instructions

else' :: Parser [Instruction]
else' =  rword "else" *> newline *> instructions
     <|> pure []

repeat :: Parser Instruction
repeat = flip Repeat <$> between (rword "begin" <* newline) (rword "repeat") instructions
                     <*> expr

switch :: Parser Instruction
switch = Switch <$> (rword "switch" *> expr <* newline)
                <*> many case'
                <*> (rword "default:" *> instructions <|> pure [])
                <*  rword "endswitch"

case' :: Parser (SmallLit, [Instruction])
case' = (,) <$> (rword "case" *> char ' '
                  *> (smallLit <* colon <* newline))
            <*> instructions

-- * Expressions
expr :: Parser Expr
expr = makeExprParser term opTable

term :: Parser Expr
term = choice (fmap try
  [ Paren <$> parens expr
  , MethodCall <$> (name <* colon) <*> qbKey <*> parens (argument `sepBy` comma)
  , BareCall <$> qbKey <*> parens (argument `sepBy` comma)
  , Member <$> (try (parens expr) <|> ELit . SmallLit . LitKey <$> name)
           <*> (op "." *> qbKey)
  , Index <$> (try (parens expr) <|> ELit . SmallLit . LitKey <$> name)
          <*> between (symbol "[") (symbol "]") expr
  , ELit <$> lit
  ]) <?> "term"

argument :: Parser (Maybe QbKey, Expr)
argument = (,) <$> optional (qbKey <* op "=") <*> expr

opTable :: [[Operator Parser Expr]]
opTable = [ [ prefix "!" Not, prefix "-" Neg, prefix "*" Deref ]
          , [ binary "*" Mul, binary "/" Div ]
          , [ binary "+" Add, binary "-" Sub ]
          , [ binary "<" Lt, binary ">" Gt, binary "<=" Lte, binary ">=" Gte ]
          , [ binary "==" Eq, binary "!=" Neq ]
          , [ binary "^" Xor ]
          , [ binary "&&" And ]
          , [ binary "||" Or ]
          ]

binary :: String -> (a -> a -> a) -> Operator Parser a
binary n f = InfixL (f <$ op n)

prefix :: String -> (a -> a) -> Operator Parser a
prefix n f = Prefix (f <$ op n)
