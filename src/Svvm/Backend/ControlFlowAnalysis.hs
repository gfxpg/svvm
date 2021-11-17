module Svvm.Backend.ControlFlowAnalysis where

import Control.Monad (join)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Svvm.Dsl hiding (CompOp (..), ElementwiseOp (..))
import Svvm.Instructions
import Svvm.Backend.Common

buildCfg :: DslState -> CFG
buildCfg s = fillPredecessors . fillSuccesors . makeBlocks $ sPgm s
  where
    fillPredecessors blocks = Seq.mapWithIndex go blocks
      where
        go bbIdx bb = bb {bbPredecessors = Seq.findIndicesL (elem bbIdx . bbSuccessors) blocks}

    fillSuccesors blocks = Seq.mapWithIndex go (insertTerminalBlock blocks)
      where
        -- Blocks ending with IEndProgram "branch" to the terminal block, which acts as a starting point for backward dataflow analysis
        insertTerminalBlock = (|> BasicBlock Seq.empty [] [])
        terminalBbIdx = Seq.length blocks
        go bbIdx (BasicBlock insts _ _)
          | _ :|> (_, IEndProgram) <- insts =
            BasicBlock insts [terminalBbIdx] []
          | bbIdx + 1 < Set.size leaders =
            let nextBbStartPc = Set.elemAt (bbIdx + 1) leaders
                thisBbEndPc = nextBbStartPc - 1
                succBbs = case Map.lookup thisBbEndPc branches of
                  Just targetPcs -> flip Set.findIndex leaders <$> targetPcs
                  _ -> [bbIdx + 1] -- if the last instruction is not a branch, we fall through to the next block
             in BasicBlock insts succBbs []
          | otherwise =
            BasicBlock insts [] []

    makeBlocks = go Seq.empty (Set.toAscList leaders)
      where
        go blocks (startPc : nextStartPc : rest) insts =
          let (thisBbInsts, nextBbInsts) = Seq.splitAt (nextStartPc - startPc) insts
              indexedInsts = Seq.zip (Seq.fromList [startPc .. nextStartPc - 1]) thisBbInsts
           in go (blocks |> BasicBlock indexedInsts [] []) (nextStartPc : rest) nextBbInsts
        go blocks [startPc] remInsts =
          let indexedInsts = Seq.zip (Seq.fromList [startPc .. startPc + Seq.length remInsts - 1]) remInsts
           in blocks |> BasicBlock indexedInsts [] []
        go blocks _ _ = blocks

    (leaders, branches) = Seq.foldrWithIndex go (Set.singleton 0, Map.empty) (sPgm s)
      where
        go pc (IBranch targetLabel) (leaders, branches) =
          let targetPc = resolveLabel pc targetLabel
           in ( Set.insert targetPc leaders,
                Map.insert pc [targetPc] branches
              )
        go pc (ICondBranch _ targetLabel) (leaders, branches) =
          let targetPcNotTaken = pc + 1
              targetPcTaken = resolveLabel pc targetLabel
           in ( Set.insert targetPcTaken $ Set.insert targetPcNotTaken leaders,
                Map.insert pc [targetPcNotTaken, targetPcTaken] branches
              )
        go _ _ acc = acc

    resolveLabel pc (Label i) = case join (sLabelTargets s Seq.!? i) of
      Just (LabelTarget pc) -> pc
      _ -> error $ "Unresolved label at instruction " <> show pc
