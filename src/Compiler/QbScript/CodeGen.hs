module Compiler.QbScript.CodeGen where

import Compiler.QbScript.AST
import Data.GH3.QB

import Control.Monad.State.Strict
import Data.ByteString(ByteString)
import Data.Packer
import Data.Word

genScriptCode :: QbScript -> ByteString
genScriptCode = runPacking 1024 . putScript

putScript :: QbScript -> Packing ()
putScript (QbScript args instrs) = do
  case args of
    Nothing -> pure ()
    (Just s) -> putStruct s
  mapM_ putInstr instrs

  putWord16BE 0x0124

data BranchState = BranchState
  { nextBranchHole :: OffsetHole
  , lastBranchHoles :: [OffsetHole]
  }

data OffsetHole = H !Int !(Hole Word16)

updateHoles :: OffsetHole -> OffsetHole -> BranchState -> BranchState
updateHoles n l (BranchState _ lastHoles) = BranchState n (l:lastHoles)

fillOffsetHole :: Int -> OffsetHole -> Packing ()
fillOffsetHole pos (H p h) = fillHole h (fromIntegral $ pos - p)

putInstr :: Instruction -> Packing ()
putInstr (BareExpr expr) = do
  putWord8 0x01
  putExpr expr
putInstr (Assign name expr) = do
  putWord8 0x01
  putName name
  putWord8 0x07
  putExpr expr
putInstr (IfElse if' elseifs else') = do
    ifHole <- putIf if'
    st <- execStateT (putElseIfs elseifs) (BranchState ifHole [])
    (BranchState nh lhs) <- execStateT (putElse else') st

    putWord16BE 0x0128

    pos <- packGetPosition

    fillOffsetHole pos nh
    mapM_ (fillOffsetHole pos) lhs
putInstr (Repeat expr body) = do
  putWord16BE 0x0120
  mapM_ putInstr body
  putWord16BE 0x0121
  putExpr expr
putInstr (Switch expr cases default') = fail "switch not yet supported"
putInstr Break = putWord16BE 0x0122

putIf :: (Expr, [Instruction]) -> Packing OffsetHole
putIf (cond, body) = do
  putWord16BE 0x0147
  pos <- packGetPosition
  offsetHole <- putHoleWord16LE
  putExpr cond
  mapM_ putInstr body
  return $ H pos offsetHole

putElseIfs :: [(Expr, [Instruction])] -> StateT BranchState Packing ()
putElseIfs = mapM_ putElseIf
  where
    putElseIf :: (Expr, [Instruction]) -> StateT BranchState Packing ()
    putElseIf (cond, body) = do
      BranchState nh _ <- get
      (nh',lh') <- lift $ do
        putWord8 0x01
        pos <- packGetPosition
        fillOffsetHole pos nh
        putWord8 0x27
        nh' <- putHoleWord16LE
        lh' <- putHoleWord16LE
        putExpr cond
        mapM_ putInstr body
        return (H (pos+1) nh', H (pos+3) lh')

      modify $ updateHoles nh' lh'

putElse :: [Instruction] -> StateT BranchState Packing ()
putElse body = do
  BranchState nh lastHoles <- get
  h' <- lift $ do
    putWord8 0x01
    pos <- packGetPosition
    fillOffsetHole pos nh
    putWord8 0x48
    nextHole <- putHoleWord16LE
    mapM_ putInstr body
    return (H (pos+1) nextHole)

  put (BranchState h' lastHoles)

putSmallLit :: SmallLit -> Packing ()
putSmallLit (LitN n) = do
  putWord8 0x17
  putWord32LE (fromIntegral n)
putSmallLit (LitH x) = do
  putWord8 0x18
  putWord32LE x
putSmallLit (LitKey n) = putName n

putLit :: Lit -> Packing ()
putLit (SmallLit s) = putSmallLit s
putLit (LitF f) = do
  putWord8 0x1A
  putFloat32LE f
putLit (LitV2 x y) = do
  putWord8 0x1F
  putFloat32LE x
  putFloat32LE y
putLit (LitV3 x y z) = do
  putWord8 0x1E
  putFloat32LE x
  putFloat32LE y
  putFloat32LE z
putLit (LitString s) = do
  putWord8 0x1B
  len <- putHoleWord32LE
  start <- packGetPosition
  mapM_ (putWord8 . fromIntegral . fromEnum) s
  putWord8 0x00
  pos <- packGetPosition
  fillHole len (fromIntegral $ pos - start)
putLit (LitWString s) = do
  putWord8 0x4C
  len <- putHoleWord32LE
  start <- packGetPosition
  mapM_ (putWord16BE . fromIntegral . fromEnum) s
  putWord16LE 0x0000
  pos <- packGetPosition
  fillHole len (fromIntegral $ pos - start)
putLit (LitDict d) = putLitDict d
putLit (LitArray a) = putLitArray a
putLit (LitStruct s) = do
  putWord8 0x4A
  len <- putHoleWord16LE
  padPos <- packGetPosition
  padTo4 padPos
  start <- packGetPosition
  putStruct s
  pos <- packGetPosition
  fillHole len (fromIntegral $ pos - start)
putLit LitPassthrough = putWord8 0x2C

padTo4 :: Int -> Packing ()
padTo4 0 = pure ()
padTo4 1 = putWord8 0 >> padTo4 2
padTo4 2 = putWord8 0 >> padTo4 3
padTo4 3 = putWord8 0
padTo4 x = padTo4 (x `mod` 4)

putName :: Name -> Packing ()
putName (Local k) = putWord8 0x2D >> putLitKey k
putName (NonLocal k) = putLitKey k

putLitKey :: QbKey -> Packing ()
putLitKey q@(QbName _) = putLitKey (canonicalise q)
putLitKey (QbCrc x) = putWord8 0x16 >> putWord32LE x

putLitDict :: Dict -> Packing ()
putLitDict (ExtendsPT entries) = do
  putWord8 0x03
  putWord16BE 0x012C
  mapM_ putDictEntry entries
  putWord16BE 0x0104
putLitDict (Dict entries) = do
  putWord8 0x03
  mapM_ putDictEntry entries
  putWord16BE 0x0104

putDictEntry :: (QbKey, Expr) -> Packing ()
putDictEntry (k, expr) = do
  putWord8 0x01
  putLitKey k
  putWord8 0x07
  putExpr expr

putLitArray :: Array -> Packing ()
putLitArray (Array entries) = do
  putWord8 0x05
  mapM_ putExpr entries
  putWord8 0x06

putExpr :: Expr -> Packing ()
putExpr (Paren expr) = do
  putWord8 0x0E
  putExpr expr
  putWord8 0x0F
putExpr (ELit lit) = putLit lit
putExpr (Neg expr) = putWord8 0x0A >> putExpr expr
putExpr (Not expr) = putWord8 0x39 >> putExpr expr
putExpr (And e1 e2) = putInfix 0x33 e1 e2
putExpr (Or e1 e2) = putInfix 0x32 e1 e2
putExpr (Xor e1 e2) = putInfix 0x34 e1 e2
putExpr (Add e1 e2) = putInfix 0x0B e1 e2
putExpr (Sub e1 e2) = putInfix 0x0A e1 e2
putExpr (Mul e1 e2) = putInfix 0x0C e1 e2
putExpr (Div e1 e2) = putInfix 0x0D e1 e2
putExpr (Lt e1 e2) = putInfix 0x12 e1 e2
putExpr (Lte e1 e2) = putInfix 0x13 e1 e2
putExpr (Eq e1 e2) = putInfix 0x07 e1 e2
putExpr (Gt e1 e2) = putInfix 0x14 e1 e2
putExpr (Gte e1 e2) = putInfix 0x15 e1 e2
putExpr (Neq e1 e2) = putInfix 0x4D e1 e2
putExpr (Deref expr) = putWord8 0x4B >> putExpr expr
putExpr (Index e1 e2) = do
  putExpr e1
  putWord8 0x05
  putExpr e2
  putWord8 0x06
putExpr (Member expr k) = do
  putExpr expr
  putWord8 0x08
  putLitKey k
putExpr (BareCall k args) = do
  putLitKey k
  mapM_ putArg args
putExpr (MethodCall n k args) = do
  putName n
  putWord8 0x42
  putLitKey k
  mapM_ putArg args
putExpr x = fail $ show x ++ " currently not supported"

putArg :: (Maybe QbKey, Expr) -> Packing ()
putArg (Nothing, expr) = putExpr expr
putArg (Just k, expr) = do
  putLitKey k
  putWord8 0x07
  putExpr expr

putInfix :: Word8 -> Expr -> Expr -> Packing ()
putInfix op e1 e2 = do
  putExpr e1
  putWord8 op
  putExpr e2

putStruct :: Struct -> Packing ()
putStruct = fail "struct codegen not yet supported"
