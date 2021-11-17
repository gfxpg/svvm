{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Svvm.Backend.InstructionSelection where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Foldable (concatMap, foldrM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word32)
import GHC.Float (castFloatToWord32)
import GHC.TypeLits
import Svvm.Backend.Common
import Svvm.Dsl.Common (DslState (..), RegBlock (rbAlignment))
import Svvm.Instructions
import Svvm.Utils.TypeUnion
import Debug.Trace (trace)

type BackendState = StateT DslState (Reader CFG)

lowerBbInsts :: BasicBlock -> BackendState BasicBlock
lowerBbInsts bb = do
  bbInsts' <- foldrM (\(pc, inst) acc -> (Seq.>< acc) . Seq.fromList . zip (repeat pc) <$> lower inst) Seq.empty (bbInsts bb)
  pure $ bb {bbInsts = bbInsts'}

niSgpr :: IsStorable t n => RegAccess -> (Int, Int) -> SRegs t n -> NativeInstOperand
niSgpr access (from, to) (SRegs t n start loc)
  | from <= to, to < tyInt n = NiSgprs loc access [start + from .. start + to]
  | otherwise = error $ "slice " <> show (from, to) <> " is out of bounds for SRegs " <> show (tyInt n)

niVgpr :: IsStorable t n => RegAccess -> (Int, Int) -> VRegs t n -> NativeInstOperand
niVgpr access (from, to) (VRegs t n start loc)
  | from <= to, to < tyInt n = NiVgprs loc access [start + from .. start + to]
  | otherwise = error $ "slice " <> show (from, to) <> " is out of bounds for VRegs " <> show (tyInt n)

niLit :: Int -> NativeInstOperand
niLit = NiLit . fromIntegral

restrictAlignment :: Int -> SRegs t n -> BackendState ()
restrictAlignment alignment reg@(SRegs t n start loc) = do
  blockStart <- case loc of
    RegLocVirt -> gets $ fst . fromJust . Map.lookupLE start . sSgprBlocks
    RegLocPhys -> pure 0 -- physical registers can be treated as belonging to a single block spanning all registers
  let offsetInBlock = start - blockStart
  when (offsetInBlock `mod` alignment /= 0) $ do
    error $
      "cannot align register " <> show reg <> " on a " <> show alignment
        <> " dword boundary: it has offset "
        <> show offsetInBlock
        <> " in the block it is allocated in"
  modify $ \s -> s {sSgprBlocks = Map.adjust (\b -> b {rbAlignment = max alignment (rbAlignment b)}) blockStart (sSgprBlocks s)}

resolveLabel :: Label -> BackendState NativeInstOperand
resolveLabel (Label i) = do
  LabelTarget targetPc <- gets $ fromJust . (`Seq.index` i) . sLabelTargets
  maybeBbIdx <- asks $ Seq.findIndexL $ \bb -> case bbInsts bb Seq.!? 0 of Just (pc, _) -> pc == targetPc; _ -> False
  case maybeBbIdx of
    Just bbIdx -> pure $ NiSymbol $ "bb" <> show bbIdx
    _ -> error $ "Cannot resolve label with target PC = " <> show targetPc

lower :: Instruction -> BackendState [Instruction]
-- Moves
lower (IVectorMoveN (dstGprs :: VRegs t n) src) = pure $
  flip fmap [0 .. dwordCount dstGprs - 1] $ \i ->
    INative "v_mov_b32" [niVgpr RegWr (i, i) dstGprs, (vsrc i @> ssrc i @> litsrc i @> fltsrc @> unionExhausted) src] []
  where
    vsrc i (srcGprs :: VRegs t n) = niVgpr RegRd (i, i) srcGprs
    ssrc i (srcGprs :: SRegs t n) = niSgpr RegRd (i, i) srcGprs
    litsrc i (lit :: Int) = NiLit (fromIntegral $ (lit `shiftR` (32 * i)) .&. 0xffffffff)
    fltsrc = NiLit . castFloatToWord32
lower (IScalarMoveN (dstGprs :: SRegs t n) src) = (ssrc @> intsrc @> fltsrc @> unionExhausted) src
  where
    ssrc (srcGprs :: SRegs t n) = do
      when (dwordCount dstGprs >= 2) $ do
        restrictAlignment 2 dstGprs
        restrictAlignment 2 srcGprs
      pure $ genMov64 (dwordCount dstGprs `quot` 2) ++ genMov32 (dwordCount dstGprs `rem` 2)
      where
        genMov64 n = flip fmap [0 .. n - 1] $ \i ->
          INative "s_mov_b64" [niSgpr RegWr (i * 2, i * 2 + 1) dstGprs, niSgpr RegRd (i * 2, i * 2 + 1) srcGprs] []
        genMov32 0 = []
        genMov32 _ =
          let lastDw = dwordCount dstGprs - 1
           in [INative "s_mov_b32" [niSgpr RegWr (lastDw, lastDw) dstGprs, niSgpr RegRd (lastDw, lastDw) srcGprs] []]
    intsrc (lit :: Int) = pure $
      flip fmap [0 .. dwordCount dstGprs - 1] $ \i ->
        let dwordi = NiLit (fromIntegral $ (lit `shiftR` (32 * i)) .&. 0xffffffff)
         in INative "s_mov_b32" [niSgpr RegWr (i, i) dstGprs, dwordi] []
    fltsrc (lit :: Float) = pure $
      flip fmap [0 .. dwordCount dstGprs - 1] $ \i ->
        INative "s_mov_b32" [niSgpr RegWr (i, i) dstGprs, NiLit $ castFloatToWord32 lit] []
-- Scalar memory ops
lower (IScalarLoadN (dstGprs :: SRegs t n) (addrGprs :: SRegs U32 2) litOffset) =
  lowerMultiDwordInst [16, 8, 4, 2, 1] (ty @n) $ \nDwords dstGprOffset -> do
    let opcode = "s_load_dword" <> if nDwords == 1 then "" else "x" <> show nDwords
    let dst = niSgpr RegWr (dstGprOffset, dstGprOffset + nDwords - 1) dstGprs
    -- "Even-aligned SGPRs are required [...] when 64-bit data is used" and with "scalar memory reads [where] the address-base comes from an SGPR-pair"
    -- "Quad-alignment is required for the data-GPR when a scalar memory read returns four or more dwords"
    restrictAlignment (min nDwords 4) dstGprs
    restrictAlignment 2 addrGprs
    pure $ INative opcode [dst, niSgpr RegRd (0, 1) addrGprs, niLit (dstGprOffset * 4 + litOffset)] []
-- Vector memory ops
lower (IBufferLoadN (dstGprs :: VRegs t n) (offGpr :: VReg U32) (srdGprs :: SRegs U32 4) litOffset) =
  lowerMultiDwordInst [4, 3, 2, 1] (ty @n) $ \nDwords dstGprOffset -> do
    let opcode = "buffer_load_dword" <> if nDwords == 1 then "" else "x" <> show nDwords
    let dst = niVgpr RegWr (dstGprOffset, dstGprOffset + nDwords - 1) dstGprs
    -- 8.1.2. Buffer Instructions: SRSRC: address should be aligned to a multiple of four SGPRs
    restrictAlignment 4 srdGprs
    pure $ INative opcode [dst, niVgpr RegRd (0, 0) offGpr, niSgpr RegRd (0, 3) srdGprs, niLit 0] ["offen", "offset:" <> show (dstGprOffset * 4 + litOffset)]
lower (IBufferStoreN (srcGprs :: VRegs t n) (offGpr :: VReg U32) (srdGprs :: SRegs U32 4) litOffset) =
  lowerMultiDwordInst [4, 3, 2, 1] (ty @n) $ \nDwords srcGprOffset -> do
    let opcode = "buffer_store_dword" <> if nDwords == 1 then "" else "x" <> show nDwords
    let src = niVgpr RegRd (srcGprOffset, srcGprOffset + nDwords - 1) srcGprs
    -- 8.1.2. Buffer Instructions: SRSRC: address should be aligned to a multiple of four SGPRs
    restrictAlignment 4 srdGprs
    pure $ INative opcode [src, niVgpr RegRd (0, 0) offGpr, niSgpr RegRd (0, 3) srdGprs, niLit 0] ["offen", "offset:" <> show (srcGprOffset * 4 + litOffset)]
lower i@(IScalarBitwiseN op dstGprs src0 src1)
  | op `tyEq` (ty @BitwAnd) = lowerSB32B64Variants "s_and" SLit64IsSplitIntoTwoInstrs dstGprs src0 src1
  | op `tyEq` (ty @BitwOr) = lowerSB32B64Variants "s_or" SLit64IsSplitIntoTwoInstrs dstGprs src0 src1
  | op `tyEq` (ty @BitwShl) = lowerSB32B64Variants "s_lshl" SLit32 dstGprs src0 src1
  | otherwise = error $ "Unsupported IScalarBiwiseN instruction: " <> show i
lower i@(IScalarArithN op (dstGprs :: SRegs t n) src0 src1)
  | op `tyEq` (ty @ArithMul) && (ty @t `tyEq` ty @I32 || ty @t `tyEq` ty @U32) = lowerS32Arith "s_mul_i32" dstGprs src0 src1
  | otherwise = error $ "Unsupported IScalarArithN instruction: " <> show i
lower i@(IVectorBitwiseN op dstGprs src0 src1)
  | op `tyEq` (ty @BitwShl) = lowerVBitwiseShl dstGprs src0 src1
  | otherwise = error $ "Unsupported IVectorBitwiseN instruction: " <> show i
lower i@(IVectorArithN op (dstGprs :: VRegs t n) src0 src1)
  -- Integer
  | op `tyEq` ty @ArithAdd && ty @t `tyEq` ty @U32 = unrollVArith32 "v_add_nc_u32" dstGprs src0 src1
  | op `tyEq` ty @ArithAdd && ty @t `tyEq` ty @I32 = unrollVArith32 "v_add_nc_i32" dstGprs src0 src1
  -- TODO: U32*U32 actually produces a U64 (v_mul_lo + v_mul_hi), but this needs to be supported in the frontend first
  | op `tyEq` ty @ArithMul && ty @t `tyEq` ty @U32 = unrollVArith32 "v_mul_lo_u32" dstGprs src0 src1
  -- Floating point
  | op `tyEq` ty @ArithAdd && ty @t `tyEq` ty @F32 = unrollVArith32 "v_add_f32" dstGprs src0 src1
  | op `tyEq` ty @ArithSub && ty @t `tyEq` ty @F32 = unrollVArith32 "v_sub_f32" dstGprs src0 src1
  | op `tyEq` ty @ArithMul && ty @t `tyEq` ty @F32 = unrollVArith32 "v_mul_f32" dstGprs src0 src1
  | otherwise = error $ "Unsupported IVectorArithN instruction: " <> show i
lower i@(IVectorCmp op (src0 :: VectorSrc t n) src1)
  | op `tyEq` ty @CmpLt && ty @t `tyEq` ty @U32 = lowerVCmp "v_cmp_lt_u32" src0 src1
  | otherwise = error $ "Unsupported IVectorCmp instruction: " <> show i
lower (ICondBranch BrNotVcc l) = (\t -> [INative "s_cbranch_vccz" [t] []]) <$> resolveLabel l
lower (IBranch l) = (\t -> [INative "s_branch" [t] []]) <$> resolveLabel l
lower IEndProgram = pure [INative "s_endpgm" [] []]
lower native@INative {} = pure [native]
lower i = error $ "Unsupported instruction " <> show i

lowerVCmp :: (IsStorable t n) => String -> VectorSrc t n -> VectorSrc t n -> BackendState [Instruction]
lowerVCmp opcode (src0 :: VectorSrc t n) src1 =
  let nisrc0 = (vsrc @> ssrc @> intsrc @> fltsrc @> unionExhausted) src0
      nisrc1 = (vsrc @> ssrc @> intsrc @> fltsrc @> unionExhausted) src1
      vsrc (srcGprs :: VRegs t n) = niVgpr RegRd (0, 0) srcGprs
      ssrc (srcGprs :: SRegs t n) = niSgpr RegRd (0, 0) srcGprs
      intsrc = NiLit . fromIntegral
      fltsrc = NiLit . castFloatToWord32
   in pure [INative opcode [NiSymbol "vcc", nisrc0, nisrc1] []]

lowerVBitwiseShl :: (IsStorable t n) => VRegs t n -> VectorSrc t n -> VectorSrc t n -> BackendState [Instruction]
lowerVBitwiseShl (dstGprs :: VRegs t n) src0 src1 = pure $
  flip fmap [0 .. dwordCount dstGprs - 1] $ \i ->
    let nisrc0 = (vsrc @> ssrc @> intsrc @> fltsrc @> unionExhausted) src0
        nisrc1 = (vsrc @> ssrc @> intsrc @> nofltsrc @> unionExhausted) src1
        vsrc (srcGprs :: VRegs t n) = niVgpr RegRd (i, i) srcGprs
        ssrc (srcGprs :: SRegs t n) = niSgpr RegRd (i, i) srcGprs
        intsrc = NiLit . fromIntegral
        fltsrc = NiLit . castFloatToWord32
        nofltsrc _ = error "Floats cannot be used to specify the bit shift"
     in INative "v_lshrrev_b32" [niVgpr RegWr (i, i) dstGprs, nisrc1, nisrc0] []

unrollVArith32 :: (IsStorable t n) => String -> VRegs t n -> VectorSrc t n -> VectorSrc t n -> BackendState [Instruction]
unrollVArith32 opcode (dstGprs :: VRegs t n) src0 src1 = pure $
  flip fmap [0 .. dwordCount dstGprs - 1] $ \i ->
    let nisrc0 = (vsrc @> ssrc @> intsrc @> fltsrc @> unionExhausted) src0
        nisrc1 = (vsrc @> ssrc @> intsrc @> fltsrc @> unionExhausted) src1
        vsrc (srcGprs :: VRegs t n) = niVgpr RegRd (i, i) srcGprs
        ssrc (srcGprs :: SRegs t n) = niSgpr RegRd (i, i) srcGprs
        intsrc = NiLit . fromIntegral
        fltsrc = NiLit . castFloatToWord32
     in INative opcode [niVgpr RegWr (i, i) dstGprs, nisrc0, nisrc1] []

lowerS32Arith :: (IsStorable t n) => String -> SRegs t n -> ScalarSrc t n -> ScalarSrc t n -> BackendState [Instruction]
lowerS32Arith opcode (dstGprs :: SRegs t n) src0 src1
  | eltSizeDw dstGprs == 1 =
    pure $
      flip fmap [0 .. dwordCount dstGprs - 1] $ \i ->
        let lowerSrc = (ssrc @> intsrc @> fltsrc @> unionExhausted)
            ssrc (regs :: SRegs t n) = niSgpr RegRd (i, i) regs
            intsrc = NiLit . fromIntegral
            fltsrc = NiLit . castFloatToWord32
         in INative opcode [niSgpr RegWr (i, i) dstGprs, lowerSrc src0, lowerSrc src1] []
  | otherwise = error $ "Unsupported IScalarArithN width: " <> show (eltSizeDw dstGprs)

lowerSB32B64Variants :: (IsStorable t n) => String -> SLitBehavior -> SRegs t n -> ScalarSrc t n -> ScalarSrc t n -> BackendState [Instruction]
lowerSB32B64Variants opcode litBehavior (dstGprs :: SRegs t n) src0 src1 = unrollScalarN litBehavior dstGprs [src0, src1] lowerSingleElt
  where
    lowerSingleElt i dst@(NiSgprs _ _ dstSgprs) [nisrc0, nisrc1] = emitNDwords (length dstSgprs)
      where
        emitNDwords 1 = pure [INative (opcode <> "_b32") [dst, nisrc0, nisrc1] []]
        emitNDwords 2 = do
          restrictAlignment 2 dstGprs
          case extractU src0 of Just (gprs0 :: SRegs t n) -> restrictAlignment 2 gprs0; _ -> pure ()
          case extractU src1 of Just (gprs1 :: SRegs t n) -> restrictAlignment 2 gprs1; _ -> pure ()
          pure [INative (opcode <> "_b64") [dst, nisrc0, nisrc1] []]
        emitNDwords n = error $ "Unsuported element width: " <> show n <> " dwords"
    lowerSingleElt _ _ srcs = error $ "Invalid source operands " <> show srcs

data SLitBehavior = SLit64IsSplitIntoTwoInstrs | SLit32

unrollScalarN ::
  (IsStorable t n) =>
  SLitBehavior ->
  SRegs t n ->
  [ScalarSrc t n] ->
  (Int -> NativeInstOperand -> [NativeInstOperand] -> BackendState [Instruction]) ->
  BackendState [Instruction]
unrollScalarN litBehavior (dstGprs@(SRegs t _ dstRegI dstLoc) :: SRegs t n) srcs gen =
  mconcat . mconcat <$> mapM genOp [0 .. opCount - 1]
  where
    opWidth = eltSizeDw dstGprs
    opCount = dwordCount dstGprs `div` opWidth
    needs64To32BitSplit = case litBehavior of
      SLit64IsSplitIntoTwoInstrs -> opWidth == 2 && any isIntOperand srcs
      SLit32 -> False
    isIntOperand = (\(_ :: SRegs t n) -> False) @> (\(_ :: Int) -> True) @> (\(_ :: Float) -> False) @> unionExhausted
    genOp (i :: Int)
      | needs64To32BitSplit = do
        lo <- gen (i * 2) (dst 1 (i * 2)) ((ssrc 1 (i * 2) @> intsrcLo i @> nofltsrc @> unionExhausted) <$> srcs)
        hi <- gen (i * 2 + 1) (dst 1 (i * 2 + 1)) ((ssrc 1 (i * 2 + 1) @> intsrcHi i @> nofltsrc @> unionExhausted) <$> srcs)
        pure [lo, hi]
      | otherwise =
        (: []) <$> gen i (dst opWidth i) ((ssrc opWidth i @> intsrcLo i @> fltsrc i @> unionExhausted) <$> srcs)
      where
        dst w i = niSgpr RegWr (i * w, (i + 1) * w - 1) dstGprs
        ssrc w i srcGprs = niSgpr RegRd (i * w, (i + 1) * w - 1) srcGprs
        intsrcLo _ = NiLit . fromIntegral
        intsrcHi _ (lit :: Int) = NiLit $ fromIntegral $ (lit `shiftR` 32) .&. 0xffffffff
        fltsrc _ = NiLit . castFloatToWord32
        nofltsrc = error "Unexpected float literal"

type InstructionVariant = Int -- e.g 4 for buffer_load_dwordx4, 1 for buffer_load_dword

lowerMultiDwordInst :: (KnownNat n, Monad m) => [InstructionVariant] -> Ty n -> (InstructionVariant -> Int -> m Instruction) -> m [Instruction]
lowerMultiDwordInst variants tyNumDwords gen = go variants numDwords []
  where
    numDwords = tyInt tyNumDwords
    go [] _ acc = pure acc
    go (step : rest) n acc
      | n >= step = gen step (numDwords - n) >>= \inst -> go (step : rest) (n - step) acc >>= \acc -> pure $ inst : acc
      | otherwise = go rest n acc

-- IBranch :: Label -> Instruction
-- ICondBranch :: BranchCond -> Label -> Instruction
-- IEndProgram :: Instruction
-- IScalarArithN :: (IsNumericTy t, IsArithOp o, KnownNat n) => Ty t -> Ty o -> ScalarDst n -> ScalarSrc n -> ScalarSrc n -> Instruction
-- IVectorArithN :: (IsNumericTy t, IsArithOp o, KnownNat n) => Ty t -> Ty o -> VectorDst n -> VectorSrc n -> VectorSrc n -> Instruction
-- IScalarBitwiseN :: (IsBitwiseOp o, KnownNat n) => Ty o -> ScalarDst n -> ScalarSrc n -> ScalarSrc n -> Instruction
-- IVectorBitwiseN :: (IsBitwiseOp o, KnownNat n) => Ty o -> VectorDst n -> VectorSrc n -> VectorSrc n -> Instruction
-- IScalarCmp :: (IsNumericTy t, IsCmpOp o) => Ty t -> Ty o -> ScalarSrc 1 -> ScalarSrc 1 -> Instruction
-- IVectorCmp :: (IsNumericTy t, IsCmpOp o) => Ty t -> Ty o -> VectorSrc 1 -> VectorSrc 1 -> Instruction
