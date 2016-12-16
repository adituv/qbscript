module Compiler.QbScript.CodeGen where

import Compiler.QbScript.AST
import Data.GH3.QB

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.ByteString(ByteString)
import Data.Packer
import Data.Word

packingBufferSize :: Int
packingBufferSize = 4096

genScriptCode :: QbScript -> ByteString
genScriptCode = runPacking packingBufferSize . putScript

putScript :: QbScript -> Packing ()
putScript (QbScript args instrs) = do
  case args of
    Nothing -> pure ()
    (Just s) -> putLit (LitStruct s)
  mapM_ putInstr instrs

  putWord16BE 0x0124

data BranchState = BranchState
  { nextBranchHole :: OffsetHole
  , lastBranchHoles :: [OffsetHole]
  }

data OffsetHole = H !Int !(Hole Word16)

updateHoles :: OffsetHole -> OffsetHole -> BranchState -> BranchState
updateHoles n l (BranchState _ lastHoles) = BranchState n (l:lastHoles)

putOffsetHole :: Packing OffsetHole
putOffsetHole = do
  pos <- packGetPosition
  hole <- putHoleWord16LE
  return $ H pos hole

fillOffsetHole :: OffsetHole -> Packing ()
fillOffsetHole (H p h) = do
  pos <- packGetPosition
  fillHole h (fromIntegral $ pos - p)

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

    fillOffsetHole nh
    mapM_ fillOffsetHole lhs
putInstr (Repeat expr body) = do
  putWord16BE 0x0120
  mapM_ putInstr body
  putWord16BE 0x0121
  putExpr expr
putInstr (Switch expr cases default') = putSwitch expr cases default'
putInstr Break = putWord16BE 0x0122
putInstr (Return Nothing) = putWord16BE 0x0129
putInstr (Return (Just (Nothing, x))) = putWord16BE 0x0129 >> putExpr x
putInstr (Return (Just (Just k, x))) = do
  putWord16BE 0x0129
  putLitKey k
  putWord8 0x07
  putExpr x

putSwitch :: Expr -> [(SmallLit, [Instruction])] -> [Instruction] -> Packing ()
putSwitch expr cases default' = do
  putWord16BE 0x013C
  putExpr expr
  putWord8 0x01
  case cases of
    [] -> putSwitchNoCases default'
    (l, body):cs -> do
      putWord16BE 0x3E49
      h <- putOffsetHole
      putSmallLit l
      mapM_ putInstr body
      st <- execStateT (mapM_ putCase cs) (BranchState h [])
      (BranchState nh lhs) <- execStateT (putDefault default') st
      putWord8 0x01
      fillOffsetHole nh
      putWord8 0x3D
      mapM_ fillOffsetHole lhs

putSwitchNoCases :: [Instruction] -> Packing ()
putSwitchNoCases body = do
  putWord16BE 0x3F49
  h <- putOffsetHole
  mapM_ putInstr body
  putWord8 0x01
  fillOffsetHole h
  putWord8 0x3D

putCase :: (SmallLit, [Instruction]) -> StateT BranchState Packing ()
putCase (l, body) = do
  BranchState nh lhs <- get
  (nh', lh') <- lift $ do
    putWord16BE 0x0149
    lh' <- putOffsetHole
    fillOffsetHole nh
    putWord16BE 0x3E49
    nh' <- putOffsetHole
    putSmallLit l
    mapM_ putInstr body
    return (nh', lh')
  put $ BranchState nh' (lh':lhs)

putDefault :: [Instruction] -> StateT BranchState Packing ()
putDefault body = do
  BranchState nh lhs <- get
  (nh',lh') <- lift $ do
    putWord16BE 0x0149
    lh' <- putOffsetHole
    fillOffsetHole nh
    putWord16BE 0x3F49
    nh' <- putOffsetHole
    mapM_ putInstr body
    return (nh', lh')
  put $ BranchState nh' (lh':lhs)


putIf :: (Expr, [Instruction]) -> Packing OffsetHole
putIf (cond, body) = do
  putWord16BE 0x0147
  offsetHole <- putOffsetHole
  putExpr cond
  mapM_ putInstr body
  return offsetHole

putElseIfs :: [(Expr, [Instruction])] -> StateT BranchState Packing ()
putElseIfs = mapM_ putElseIf
  where
    putElseIf :: (Expr, [Instruction]) -> StateT BranchState Packing ()
    putElseIf (cond, body) = do
      BranchState nh _ <- get
      (nh',lh') <- lift $ do
        putWord8 0x01
        fillOffsetHole nh
        putWord8 0x27
        nh' <- putOffsetHole
        lh' <- putOffsetHole
        putExpr cond
        mapM_ putInstr body
        return (nh', lh')

      modify $ updateHoles nh' lh'

putElse :: [Instruction] -> StateT BranchState Packing ()
putElse body = do
  BranchState nh lastHoles <- get
  h' <- lift $ do
    putWord8 0x01
    fillOffsetHole nh
    putWord8 0x48
    nextHole <- putOffsetHole
    mapM_ putInstr body
    return nextHole

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
  padTo4
  start <- packGetPosition
  runReaderT (putStruct s) start
  pos <- packGetPosition
  fillHole len (fromIntegral $ pos - start)
putLit LitPassthrough = putWord8 0x2C

padTo4 :: Packing ()
padTo4 = do
    pos <- packGetPosition
    padTo4' pos
  where
    padTo4' 0 = pure ()
    padTo4' 1 = putWord8 0 >> padTo4' 2
    padTo4' 2 = putWord8 0 >> padTo4' 3
    padTo4' 3 = putWord8 0
    padTo4' x = padTo4' (x `mod` 4)

putName :: Name -> Packing ()
putName (Local k) = putWord8 0x2D >> putLitKey k
putName (NonLocal k) = putLitKey k

putLitKey :: QbKey -> Packing ()
putLitKey q@(QbName _) = putLitKey (canonicalise q)
putLitKey (QbCrc x) = putWord8 0x16 >> putWord32LE x

putLitDict :: Dict -> Packing ()
putLitDict (Dict entries) = do
  putWord8 0x03
  mapM_ putDictEntry entries
  putWord16BE 0x0104

putDictEntry :: (Maybe QbKey, Expr) -> Packing ()
putDictEntry (Nothing, expr) = do
  putWord8 0x01
  putExpr expr
putDictEntry (Just k, expr) = do
  putWord8 0x01
  putLitKey k
  putWord8 0x07
  putExpr expr

putLitArray :: Array -> Packing ()
putLitArray (Array []) = putWord16BE 0x0506
putLitArray (Array (x:xs)) = do
  putWord8 0x05
  putExpr x
  mapM_ (\e -> putWord8 0x09 >> putExpr e) xs
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

-- TODO: move struct inside the ReaderT so that nested structs don't
--       reset the position
putStruct :: Struct -> ReaderT Int Packing ()
putStruct (Struct items) = do
  lift $ putWord32BE 0x00010000
  firstEntry <- lift putHoleWord32BE
  lastHole   <- flip execStateT firstEntry
              . mapM_ putStructItem
              $ items
  lift $ fillHole lastHole 0

putStructItem :: StructItem -> StateT (Hole Word32) (ReaderT Int Packing) ()
putStructItem (StructItem ty key value) = do
  prevHole <- get
  startPos <- ask
  hole <- lift . lift $ do
    padTo4
    pos <- packGetPosition
    fillHole prevHole (fromIntegral $ pos - startPos)
    putQbType ty
    putQbKey key
    vh <- putQbValue value
    nh <- putHoleWord32BE
    pos' <- packGetPosition
    case vh of
      Nothing -> pure ()
      Just h -> fillHole h (fromIntegral $ pos' - startPos)
    return nh
  lift $ putQbValueData value
  put hole
  return ()

putQbType :: QbType -> Packing ()
putQbType QbTInteger       = putWord32BE 0x00810000
putQbType QbTFloat         = putWord32BE 0x00820000
putQbType QbTString        = putWord32BE 0x00830000
putQbType QbTWString       = putWord32BE 0x00840000
putQbType QbTVector2       = putWord32BE 0x00850000
putQbType QbTVector3       = putWord32BE 0x00860000
putQbType QbTStruct        = putWord32BE 0x008A0000
putQbType (QbTArray _)     = putWord32BE 0x008C0000
putQbType QbTKey           = putWord32BE 0x008D0000
putQbType QbTKeyRef        = putWord32BE 0x009A0000
putQbType QbTStringPointer = putWord32BE 0x009B0000
putQbType QbTStringQs      = putWord32BE 0x009C0000

putQbKey :: QbKey -> Packing ()
putQbKey q@(QbName _) = putQbKey (canonicalise q)
putQbKey (QbCrc k) = putWord32BE k

putQbValue :: QbValue -> Packing (Maybe (Hole Word32))
putQbValue (QbInteger n)       = putWord32BE (fromIntegral n) >> pure Nothing
putQbValue (QbFloat f)         = putFloat32BE f >> pure Nothing
putQbValue (QbString _)        = Just <$> putHoleWord32BE
putQbValue (QbWString _)       = Just <$> putHoleWord32BE
putQbValue  QbVector2{}        = Just <$> putHoleWord32BE
putQbValue  QbVector3{}        = Just <$> putHoleWord32BE
putQbValue (QbStruct _)        = Just <$> putHoleWord32BE
putQbValue (QbArray _)         = Just <$> putHoleWord32BE
putQbValue (QbKey k)           = putQbKey k >> pure Nothing
putQbValue (QbKeyRef k)        = putQbKey k >> pure Nothing
putQbValue (QbStringPointer k) = putQbKey k >> pure Nothing
putQbValue (QbStringQs k)      = putQbKey k >> pure Nothing

putQbValueData :: QbValue -> ReaderT Int Packing ()
putQbValueData (QbString s) = lift $ do
  mapM_ (putWord8 . fromIntegral . fromEnum) s
  putWord8 0x00
putQbValueData (QbWString s) = lift $ do
  mapM_ (putWord16BE . fromIntegral . fromEnum) s
  putWord16BE 0x0000
putQbValueData (QbVector2 x y) = lift $ putWord32BE 0x00010000 >> putFloat32BE x >> putFloat32BE y
putQbValueData (QbVector3 x y z) = lift $ putWord32BE 0x00010000 >> putFloat32BE x >> putFloat32BE y >> putFloat32BE z
putQbValueData (QbStruct s) = putStruct s
putQbValueData (QbArray a) = putQbArray a
putQbValueData _           = pure ()

putQbArray :: QbArray -> ReaderT Int Packing ()
putQbArray (QbArr t []) = lift $ do
  putQbArrayType t
  putWord32BE 0 -- array length
putQbArray (QbArr t [x]) = do
  start <- ask
  lift $ do
    putQbArrayType t
    putWord32BE 1 -- array length
    vh <- putQbValue x
    case vh of
      Nothing -> pure ()
      Just h -> do
        pos <- packGetPosition
        fillHole h (fromIntegral $ pos - start)
  putQbValueData x
putQbArray (QbArr t xs) = do
  start <- ask
  lift $ do
    putQbArrayType t
    putWord32BE (fromIntegral $ length xs)
    pos <- packGetPosition
    putWord32BE (fromIntegral $ pos - start + 4)

  holes <- lift $ mapM putQbValue xs
  zipWithM_ putQbArrayData xs holes

putQbArrayType :: QbType -> Packing ()
putQbArrayType QbTInteger       = putWord32BE 0x00010100
putQbArrayType QbTFloat         = putWord32BE 0x00010200
putQbArrayType QbTString        = putWord32BE 0x00010300
putQbArrayType QbTWString       = putWord32BE 0x00010400
putQbArrayType QbTVector2       = putWord32BE 0x00010500
putQbArrayType QbTVector3       = putWord32BE 0x00010600
putQbArrayType QbTStruct        = putWord32BE 0x00010A00
putQbArrayType (QbTArray _)     = putWord32BE 0x00010C00
putQbArrayType QbTKey           = putWord32BE 0x00010D00
putQbArrayType QbTKeyRef        = putWord32BE 0x00011A00
putQbArrayType QbTStringPointer = putWord32BE 0x00011B00
putQbArrayType QbTStringQs      = putWord32BE 0x00011C00

putQbArrayData :: QbValue -> Maybe (Hole Word32) -> ReaderT Int Packing ()
putQbArrayData v Nothing = putQbValueData v
putQbArrayData v (Just h) = do
  start <- ask
  pos <- lift packGetPosition
  lift $ fillHole h (fromIntegral $ pos - start)
  putQbValueData v
