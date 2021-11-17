{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Backend.RegisterAllocationSpec where

import Control.Monad (forM_, replicateM)
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Word (Word32)
import Svvm.Backend.Common
import Svvm.Backend.ControlFlowAnalysis
import Svvm.Backend.InstructionSelection
import Svvm.Backend.RegisterAllocation
import Svvm.Driver
import Svvm.Dsl
import Svvm.Instructions
import qualified Svvm.Utils.GraphvizDot as G
import Test.HUnit (assertEqual)
import Test.Hspec
import TestHelpers
import Svvm.Utils.TypeUnion
import Prelude hiding (Num (..), fromInteger, fromRational, not, (!!), (&&), (<), (||))

runRegAllocSpec file (pgm :: Dsl ()) = do
  let tr = translateProgram pgm
  let actual =
        G.directedGraph "Test" $
          showDotCfgSubgraph (trMachineCfg tr) "Virtual" "bb_virt_regs"
            <> showDotCfgSubgraph (trFinalCfg tr) "Renamed" "bb_phys_regs"
            <> showDotRegAllocState (trDsl tr) (trRegAlloc tr)

  expected <- readFile file
  assertEqual actual expected actual

spec :: Spec
spec = describe "register allocation" $ do
  it "computes register liveness and picks physical registers" $ do
    runRegAllocSpec "test/Backend/GFX10/Alloc.dot" $ do
      s_size :: SRegs U32 3 <- alloc
      v_in_offset :: VRegs U32 2 <- alloc

      s_untouchable :: SRegs I32 3 <- allocPhysical 0 -- physical s[0:2] are allocated and cannot be used by virtual registers
      v_untouchable :: VRegs I32 3 <- allocPhysical 1 -- physical v[1:3] are allocated and cannot be used by virtual registers
      s_size .= 2

      s :: SRegs U32 2 <- alloc

      while_ (v_in_offset !! 0 < s_size !! 0) $ do
        v_in_offset .= v_in_offset + 4

        if_ (v_in_offset !! 1 < s !! 0) $ do
          v :: VRegs U32 1 <- alloc
          s .= slice @2 @0 s_size
          v .= s !! 0

      s1 :: SReg U32 <- alloc
      s1 .= s !! 0

      endpgm
