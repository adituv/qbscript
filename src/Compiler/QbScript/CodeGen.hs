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
putInstr (IfElse if' elseifs else') = do
    ifHole <- putIf if'
    st <- execStateT (putElseIfs elseifs) (BranchState ifHole [])
    (BranchState nh lhs) <- execStateT (putElse else') st

    putWord16BE 0x0128

    pos <- packGetPosition

    fillOffsetHole pos nh
    mapM_ (fillOffsetHole pos) lhs
putInstr _ = error "TODO"

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

putExpr :: Expr -> Packing ()
putExpr = error "TODO"

putStruct :: Struct -> Packing ()
putStruct = error "TODO"
