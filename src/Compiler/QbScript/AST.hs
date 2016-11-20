module Compiler.QbScript.AST where

import Data.GH3.QB(QbKey, Struct)

import Data.Int(Int32)
import Data.Map(Map)
import Data.String.Unicode(UString)
import Data.Word(Word32)

data QbScript = QbScript (Maybe Struct) [Instruction] deriving (Show, Eq)

data Instruction = BareExpr Expr
                 | Assign Name Expr
                 | IfElse [(Expr, [Instruction])] [Instruction]     -- ^ IfElse [(Condition, Body)] [ElseBody]
                 | Repeat Expr [Instruction]                        -- ^ Repeat xExpr [Body]
                 | Switch Expr [(SmallLit, [Instruction])] [Instruction] -- ^ Switch Expr [(Case, Body)] [DefaultBody]
                 deriving (Show, Eq)

data Ty = TInt
        | THex -- ^ Not understood.. unsigned int?
        | TFloat
        | TKey Ty
        | TString
        | TWString
        | TDict
        | TArray Ty
        | TStruct
        deriving(Show, Eq)

-- | "Small" literals that can be used in case expressions
data SmallLit = LitN Int32
              | LitH Word32
              | LitKey Name
              deriving (Show, Eq)

data Lit = SmallLit SmallLit
         | LitF Float
         | LitString String
         | LitWString UString
         | LitDict Dict
         | LitArray Array
         | LitStruct Struct
         | LitPassthrough
         deriving (Show, Eq)

data Name = Local QbKey
          | NonLocal QbKey
          deriving (Show, Eq)

data Dict = ExtendsPT (Map QbKey Expr)
          | Dict (Map QbKey Expr)
          deriving (Show, Eq)

data Array = Array [Expr]
           deriving (Show, Eq)

data Expr = Paren Expr
          | ELit Lit
          | Neg Expr
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Xor Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Lt Expr Expr
          | Lte Expr Expr
          | Eq Expr Expr
          | Gt Expr Expr
          | Gte Expr Expr
          | Neq Expr Expr
          | Deref Expr
          | Index Expr Expr
          | Member Expr QbKey
          | BareCall QbKey [(Maybe QbKey, Expr)]
          | MethodCall Name QbKey [(Maybe QbKey, Expr)]
          -- TODO: Unknown operands
          | Random
          | Random2
          | RandomRange
          | RandomRange2
          | RandomNoRepeat
          | RandomPermute
          deriving (Show, Eq)
