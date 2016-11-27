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
qbScript = QbScript <$> (rword "script" *> parens (optional struct) <* newline)
                    <*> many instruction
                    <*  rword "endscript"

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
dict = braces $ optional newline *>
            (ExtendsPT <$> (passthrough *> comma *> optional newline *> dict')
         <|> Dict <$> dict')
  where
    dict' :: Parser [(QbKey, Expr)]
    dict' = entry `sepBy` (comma <* optional newline)
         <*  optional newline

    entry :: Parser (QbKey, Expr)
    entry = (,) <$> qbKey <*> (colon *> expr)

array :: Parser Array
array = Array <$> brackets (optional newline *> expr `sepBy` (comma <* optional newline)
                         <* optional newline)

-- ** Structs

struct :: Parser Struct
struct = Struct <$> braces (optional newline
                         *> structItem `sepBy` try (semicolon <* optional newline <* notFollowedBy (symbol "}"))
                         <* semicolon <* optional newline)

structItem :: Parser StructItem
structItem = do
  ty <- qbType
  k <- QbCrc 0 <$ try (symbol "_") <|> qbKey
  equals
  v <- qbValue ty
  return $ StructItem ty k v

qbType :: Parser QbType
qbType = choice (fmap try
           [ QbTInteger <$ symbol "int"
           , QbTFloat <$ symbol "float"
           , QbTString <$ symbol "string"
           , QbTWString <$ symbol "wstring"
           , QbTVector2 <$ symbol "vector2"
           , QbTVector3 <$ symbol "vector3"
           , QbTStruct <$ symbol "struct"
           , QbTArray <$ symbol "array"
           , QbTKey <$ symbol "qbkey"
           , QbTKeyRef <$ symbol "qbkeyref"
           , QbTStringPointer <$ symbol "stringptr"
           , QbTStringQs <$ symbol "stringqs"
           ]) <?> "struct item type"


qbValue :: QbType -> Parser QbValue
qbValue QbTInteger = QbInteger . fromInteger <$> (try (char '0' *> char 'x') *> hexadecimal
                                             <|> decimal)
qbValue QbTFloat = QbFloat <$> float
qbValue QbTString = QbString <$> narrowString
qbValue QbTWString = QbString <$> wideString
qbValue QbTVector2 = parens (QbVector2 <$> float <*> (comma *> float))
qbValue QbTVector3 = parens (QbVector3 <$> float <*> (comma *> float) <*> (comma *> float))
qbValue QbTStruct = QbStruct <$> struct
qbValue QbTArray = QbArray <$> qbArray
qbValue QbTKey = QbKey <$> qbKey
qbValue QbTKeyRef = QbKeyRef <$> qbKey
qbValue QbTStringPointer = QbStringPointer <$> qbKey
qbValue QbTStringQs = QbStringQs <$> qbKey


qbArray :: Parser QbArray
qbArray = brackets $ choice [ QbArr t <$> qbValue t `sepBy` (comma <* optional newline)
                            | t <- qbTypes]
  where
    qbTypes :: [QbType]
    qbTypes = [ QbTInteger, QbTFloat, QbTString, QbTWString, QbTVector2, QbTVector3, QbTStruct
              , QbTArray, QbTKey, QbTKeyRef, QbTStringPointer, QbTStringQs]

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
  , Break <$ rword "break"
  , Return <$> (rword "return" *> optional (parens argument <|> argument))
  , BareExpr <$> expr
  ]) <* lineTerm

ifelse :: Parser Instruction
ifelse = IfElse <$> if'
                <*> many elseif
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
                     <*> parens expr

switch :: Parser Instruction
switch = Switch <$> (rword "switch" *> expr <* newline)
                <*> many case'
                <*> (rword "default:" *> newline *> instructions <|> pure [])
                <*  rword "endswitch"

case' :: Parser (SmallLit, [Instruction])
case' = (,) <$> (rword "case" *> (smallLit <* colon <* newline))
            <*> instructions

-- * Expressions
expr :: Parser Expr
expr = makeExprParser term opTable

term :: Parser Expr
term = choice (fmap try
  [ Paren <$> parens expr
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
