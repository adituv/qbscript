module Compiler.QbScript.Parser where

import           Prelude hiding(repeat)

import           Compiler.QbScript.AST
import           Data.GH3.QB

import           Control.Monad(void)
import           Data.Char(toLower)
import           Data.Scientific(toRealFloat)
import           Data.Word(Word32)
import           Text.Megaparsec hiding(newline)
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text.Lazy(Parser)

-- * Lexer

alpha :: [Char]
alpha = ['A'..'Z'] ++ ['a'..'z']

ident :: [Char]
ident = alpha ++ ['0'..'9'] ++ "_"

identChar :: Parser Char
identChar = oneOf ident

opChar :: Parser Char
opChar = oneOf ".:+-*/=<>&|^"

spaceConsumer :: Parser ()
spaceConsumer = L.space (() <$ spaceChar)
                        (L.skipLineComment "//")
                        (L.skipBlockComment "/*" "*/")

reservedWords :: [String]
reservedWords = [ "if", "else", "elseif", "repeat", "script", "switch", "case"
                , "default", "break", "random", "random2", "randomrange"
                , "randomrange2", "randompermute", "randomshuffle", "not"
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

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

newline :: Parser ()
newline = void $ some (lexeme eol)

comma :: Parser ()
comma = void $ symbol ","

colon :: Parser ()
colon = void $ symbol ":"

semicolon :: Parser ()
semicolon = void $ symbol ";"

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
identifier = (lexeme . try) (((:) <$> oneOf alpha <*> many identChar) >>= check)
  where
    check x = if fmap toLower x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

-- | Workaround because L.charLiteral does not parse \&
charLiteral :: Parser Char
charLiteral = optional (try $ char '\\' *> char '&') *> L.charLiteral

narrowString :: Parser String
narrowString = lexeme $ char '\'' *> manyTill (charLiteral >>= check) (char '\'')
  where
    check x = if fromEnum x < 128
                then return x
                else fail $ "character " ++ show x ++ " out of range in narrow string"

wideString :: Parser String
wideString = lexeme $ char '"' *> manyTill charLiteral (char '"')

passthrough :: Parser ()
passthrough = void (symbol "<...>")

-- * Parser

qbScript :: Parser QbScript
qbScript = QbScript <$> (rword "script" *> parens (optional struct))
                    <*> braces (many instruction)

-- * Literals

lit :: Parser Lit
lit = choice $ fmap try
        [ LitF <$> float
        , SmallLit <$> smallLit
        , parens (LitV2 <$> float <*> (comma *> float))
        , parens (LitV3 <$> float <*> (comma *> float) <*> (comma *> float))
        , LitString <$> narrowString
        , LitWString <$> wideString
        , LitDict <$> dict
        , LitArray <$> array
        , LitStruct <$> struct
        , LitPassthrough <$ passthrough
        ]

smallLit :: Parser SmallLit
smallLit = choice $ fmap try
             [ LitH . fromInteger <$> hexadecimal
             , LitN . fromInteger <$> decimal
             , LitKey <$> name
             ]

name :: Parser Name
name =  Local <$> (symbol "%" *> qbKey)
    <|> NonLocal <$> qbKey

qbKey :: Parser QbKey
qbKey =  QbCrc <$> checksum
     <|> QbName <$> identifier

dict :: Parser Dict
dict = Dict <$> braces dict'
  where
    dict' :: Parser [(Maybe QbKey, Expr)]
    dict' = entry `sepBy` comma

    entry :: Parser (Maybe QbKey, Expr)
    entry = do
      k <- optional (try $ qbKey <* colon)
      v <- expr
      return (k,v)

array :: Parser Array
array = Array <$> brackets (expr `sepBy` comma)

-- ** Structs

struct :: Parser Struct
struct = Struct <$> braces (structItem `endBy` semicolon)

structItem :: Parser StructItem
structItem = do
  ty <- qbType
  k <- QbCrc 0 <$ try (symbol "_") <|> qbKey
  equals
  v <- qbValue ty
  return $ StructItem ty k v

qbType :: Parser QbType
qbType = choice (fmap try
           [ QbTInteger <$ rword "int"
           , QbTFloat <$ rword "float"
           , QbTString <$ rword "string"
           , QbTWString <$ rword "wstring"
           , QbTVector2 <$ rword "vector2"
           , QbTVector3 <$ rword "vector3"
           , QbTStruct <$ rword "struct"
           , QbTArray <$> (rword "array" *> angles qbType) 
           , QbTKey <$ rword "qbkey"
           , QbTKeyRef <$ rword "qbkeyref"
           , QbTStringPointer <$ rword "stringptr"
           , QbTStringQs <$ rword "stringqs"
           ]) <?> "struct item type"


qbValue :: QbType -> Parser QbValue
qbValue QbTInteger = QbInteger . fromInteger <$> (try (char '0' *> char 'x') *> hexadecimal
                                             <|> decimal)
qbValue QbTFloat = QbFloat <$> float
qbValue QbTString = QbString <$> narrowString
qbValue QbTWString = QbWString <$> wideString
qbValue QbTVector2 = parens (QbVector2 <$> float <*> (comma *> float))
qbValue QbTVector3 = parens (QbVector3 <$> float <*> (comma *> float) <*> (comma *> float))
qbValue QbTStruct = QbStruct <$> struct
qbValue (QbTArray t) = QbArray <$> qbArray t
qbValue QbTKey = QbKey <$> qbKey
qbValue QbTKeyRef = QbKeyRef <$> qbKey
qbValue QbTStringPointer = QbStringPointer <$> qbKey
qbValue QbTStringQs = QbStringQs <$> qbKey


qbArray :: QbType -> Parser QbArray
qbArray t = brackets $ QbArr t <$> qbValue t `sepBy` comma

-- * Instructions

instructions :: Parser [Instruction]
instructions = many instruction

instruction :: Parser Instruction
instruction = choice (fmap try
  [ Assign <$> name <*> (equals *> expr) <* semicolon
  , ifelse
  , repeat
  , switch
  , Break <$ rword "break" <* semicolon
  , Return <$> (rword "return" *> optional (parens argument <|> argument)) <* semicolon
  , BareExpr <$> expr <* semicolon
  ])

ifelse :: Parser Instruction
ifelse = IfElse <$> if'
                <*> many elseif
                <*> else'

if' :: Parser (Expr, [Instruction])
if' = (,) <$> (rword "if" *> parens expr)
          <*> braces instructions

elseif :: Parser (Expr, [Instruction])
elseif = (,) <$> (rword "elseif" *> parens expr)
             <*> braces instructions

else' :: Parser [Instruction]
else' =  (rword "else" *> braces instructions)
     <|> pure []

repeat :: Parser Instruction
repeat = Repeat <$> (rword "repeat" *> optional (parens expr))
                <*> braces instructions

switch :: Parser Instruction
switch = do
    rword "switch"
    sw <- parens expr
    (c, d) <- braces $ do
                   c' <- many case'
                   d' <- def
                   pure (c', d')
    pure $ Switch sw c d
  where
    case' = (,) <$> (rword "case" *> smallLit <* colon)
                <*> instructions
    def =  (rword "default" *> colon *> instructions)
       <|> pure []

-- * Expressions
expr :: Parser Expr
expr = makeExprParser term opTable

term :: Parser Expr
term = choice (fmap try
  [ Paren <$> expr
  , MethodCall <$> (name <* colon) <*> qbKey <*> parens (try argument `sepBy` comma)
  , BareCall <$> qbKey <*> parens (try argument `sepBy` comma)
  , ELit <$> lit
  ]) <?> "term"

argument :: Parser (Maybe QbKey, Expr)
argument = (,) <$> optional (try $ qbKey <* op "=") <*> expr

opTable :: [[Operator Parser Expr]]
opTable = [ [ prefix "*" Deref ]
          , [ member, index ]
          , [ prefix "!" Not, prefix "-" Neg ]
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

member :: Operator Parser Expr
member = Postfix $ flip Member <$> (op "." *> qbKey)

index :: Operator Parser Expr
index = Postfix $ flip Index <$> (op "[" *> expr <* op "]")
