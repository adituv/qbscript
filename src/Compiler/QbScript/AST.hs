module Compiler.QbScript.AST where

import Data.GH3.QB(QbKey, Struct)

import Control.Arrow((***), second)
import Data.Int(Int32)
import Data.String.Unicode(UString)
import Data.Word(Word32)

data QbScript = QbScript (Maybe Struct) [Instruction] deriving (Show, Eq)

data Instruction = BareExpr Expr
                 | Assign Name Expr
                 | IfElse (Expr, [Instruction]) [(Expr, [Instruction])] [Instruction]     -- ^ IfElse [(Condition, Body)] [ElseBody]
                 | Repeat Expr [Instruction]                        -- ^ Repeat xExpr [Body]
                 | Switch Expr [(SmallLit, [Instruction])] [Instruction] -- ^ Switch Expr [(Case, Body)] [DefaultBody]
                 | Break
                 | Return (Maybe (Maybe QbKey, Expr))
                 deriving (Show, Eq)

data Ty = TInt
        | THex -- ^ Not understood.. unsigned int?
        | TFloat
        | TVec2
        | TVec3
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
         | LitV2 Float Float
         | LitV3 Float Float Float
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

data Dict = ExtendsPT [(QbKey, Expr)]
          | Dict [(QbKey, Expr)]
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

-- | Replace @Neg [signed literal]@ with a literal of opposite sign
compressNegs :: Instruction -> Instruction
compressNegs (BareExpr x) = BareExpr $ negLit x
compressNegs (Assign n x) = Assign n $ negLit x
compressNegs (IfElse if' elseifs else') =
  IfElse (negLit *** fmap compressNegs $ if')
         (fmap (negLit *** fmap compressNegs) elseifs)
         (fmap compressNegs else')
compressNegs (Repeat x body) = Repeat (negLit x) (fmap compressNegs body)
compressNegs (Switch x cases defaults) = Switch (negLit x)
                                            (fmap (second (fmap compressNegs)) cases)
                                            (fmap compressNegs defaults)
compressNegs Break = Break
compressNegs (Return (Just (k, x))) = Return (Just (k, negLit x))
compressNegs (Return Nothing) = Return Nothing

negLit :: Expr -> Expr
negLit (Paren x) = Paren (negLit x)
negLit (Neg x) = case negLit x of
  Neg (ELit (LitF y))            -> ELit (LitF (-y))
  Neg (ELit (SmallLit (LitN y))) -> ELit (SmallLit (LitN (-y)))
  y                              -> y
negLit (Not x) = Not (negLit x)
negLit (And x y) = And (negLit x) (negLit y)
negLit (Or x y) = Or (negLit x) (negLit y)
negLit (Xor x y) = Xor (negLit x) (negLit y)
negLit (Add x y) = Add (negLit x) (negLit y)
negLit (Sub x y) = Sub (negLit x) (negLit y)
negLit (Mul x y) = Mul (negLit x) (negLit y)
negLit (Div x y) = Div (negLit x) (negLit y)
negLit (Lt x y) = Lt (negLit x) (negLit y)
negLit (Lte x y) = Lte (negLit x) (negLit y)
negLit (Gt x y) = Gt (negLit x) (negLit y)
negLit (Neq x y) = Neq (negLit x) (negLit y)
negLit (Deref x) = Deref (negLit x)
negLit (Index x y) = Index (negLit x) (negLit y)
negLit (Member x y) = Member (negLit x) y
negLit (BareCall q args) = BareCall q (fmap (second negLit) args)
negLit (MethodCall n q args) = MethodCall n q (fmap (second negLit) args)
negLit x = x
