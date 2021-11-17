{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.InstructionSelectionSpec where

import Control.Monad (forM_, replicateM)
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Word (Word32)
import Svvm.Backend.Common
import Svvm.Backend.ControlFlowAnalysis
import Svvm.Backend.InstructionSelection
import Svvm.Driver
import Svvm.Dsl
import Svvm.Instructions
import Test.HUnit.Lang (assertEqual)
import Test.Hspec
import TestHelpers
import Svvm.Utils.TypeUnion
import Prelude hiding (Num (..), fromInteger, fromRational, not, (!!), (&&), (<), (||))

runLoweringSpec file (pgm :: Dsl ()) = do
  let pgmState = execState pgm initDslState
  let cfg = buildCfg pgmState
  let (loweredCfg, pgmState') = flip runReader cfg $ flip runStateT pgmState $ mapM lowerBbInsts cfg
  let (machineToIrPc, machineCfg) = recomputeInstructionPcs loweredCfg
  let showRegBlock (rbStart, RegBlock {rbSize, rbAlignment}) =
        "---- Block [" <> show rbStart <> ":" <> show (rbStart + rbSize - 1) <> "], alignment " <> show rbAlignment
  let actual =
        showMachineInstsWithIr showInstructionDetailed pgmState' machineToIrPc machineCfg
          <> "\n-- Virtual SGPRs (%s[...]):"
          <> mconcat (("\n" <>) . showRegBlock <$> Map.toList (sSgprBlocks pgmState'))
          <> "\n-- Virtual VGPRs (%v[...]):"
          <> mconcat (("\n" <>) . showRegBlock <$> Map.toList (sVgprBlocks pgmState'))
          <> "\n"

  expected <- readFile file
  assertEqual actual expected actual -- the first argument is printed before "expected X, but got Y"

spec :: Spec
spec = describe "backend" $ do
  it "lowers IVectorMoveN" $ do
    runLoweringSpec "test/Backend/GFX10/IVectorMoveN.s" $ do
      v1 :: VReg I32 <- alloc
      v4 :: VRegs I32 4 <- alloc
      s2 :: SRegs I32 2 <- allocPhysical 6 -- 6:7
      v1 .= 13
      v4 !! 0 .= v1
      slice @2 @0 v4 .= 0x1e1e1e1edadadada
      slice @2 @2 v4 .= s2

  it "lowers IScalarMoveN" $ do
    runLoweringSpec "test/Backend/GFX10/IScalarMoveN.s" $ do
      s1 :: SReg I32 <- allocPhysical 8 -- s8
      s5 :: SRegs I32 5 <- alloc
      slice @2 @0 s5 .= 1
      s5 !! 2 .= s1
      s5' :: SRegs I32 5 <- alloc
      s5' .= s5

  it "lowers IScalarLoadN" $ do
    runLoweringSpec "test/Backend/GFX10/IScalarLoadN.s" $ do
      sdst :: SRegs U32 47 <- alloc
      saddr :: SRegs U32 2 <- allocPhysical 6 -- s[6:7]
      s_load_dwordN sdst saddr 4

  it "lowers IBufferLoadN/IBufferStoreN" $ do
    runLoweringSpec "test/Backend/GFX10/IBufferLoadStoreN.s" $ do
      srd :: SRegs U32 4 <- alloc
      voff :: VReg U32 <- alloc
      vdst :: VRegs I32 9 <- alloc
      buffer_load_dwordN vdst voff srd 0

      srd_out :: SRegs U32 4 <- alloc
      voff_out :: VReg U32 <- alloc
      vdst_out :: VRegs I32 3 <- alloc
      buffer_store_dwordN vdst_out voff_out srd_out 0

  it "lowers IScalarBitwiseN" $ do
    runLoweringSpec "test/Backend/GFX10/IScalarBitwiseN.s" $ do
      s01 :: SRegs I32 2 <- alloc
      s01 .= s01 & 0xffff
      s25 :: SRegs U64 4 <- alloc
      s25 .= 0x1e1e1e1edadadada & s25
      s67 :: SRegs U64 2 <- alloc
      s67 .= s67 & slice @2 @0 s25
      s25 .= s25 << 2
      s01 !! 0 .= s01 !! 0 << s01 !! 0

  it "lowers IScalarArithN" $ do
    runLoweringSpec "test/Backend/GFX10/IScalarArithN.s" $ do
      s01 :: SRegs I32 2 <- alloc
      s01 .= s01 * 5
      s01 !! 0 .= s01 !! 0 * s01 !! 1

  it "lowers IVectorBitwiseN" $ do
    runLoweringSpec "test/Backend/GFX10/IVectorBitwiseN.s" $ do
      v01 :: VRegs U32 2 <- alloc
      s01 :: SRegs U32 2 <- alloc
      v01 .= v01 << s01
      v01 !! 0 .= v01 !! 0 << 10

  it "lowers IVectorArithN" $ do
    runLoweringSpec "test/Backend/GFX10/IVectorArithN.s" $ do
      v01 :: VRegs U32 2 <- alloc
      s01 :: SRegs U32 2 <- alloc
      v01 .= v01 + s01
      v01 !! 0 .= v01 !! 0 + 10
      -- TODO: U64 <- U32*U32
      v01 !! 0 .= v01 !! 0 * s01 !! 0
      -- I32
      let vi32 = cast @I32 v01
      vi32 .= vi32 + cast @I32 s01
      -- F32
      let vf32 = cast @F32 (v01 !! 0)
      vf32 .= vf32 + cast @F32 s01 !! 0
      vf32 .= vf32 + (3.0 + 0.13)
      vf32 .= vf32 - vf32
      vf32 .= vf32 * vf32

  it "lowers IBranchCond" $ do
    runLoweringSpec "test/Backend/GFX10/IBranchCond.s" $ do
      s_size :: SReg U32 <- alloc
      v_in_offset :: VReg U32 <- alloc

      while_ (v_in_offset < s_size) $ do
        v_in_offset .= v_in_offset + 4

      endpgm
