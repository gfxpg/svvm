module Svvm.Driver where

import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Svvm.Backend.Common
import Svvm.Backend.ControlFlowAnalysis
import Svvm.Backend.InstructionSelection
import Svvm.Backend.RegisterAllocation
import Svvm.Dsl (Dsl, DslState (..))
import Svvm.Dsl.Common
import Svvm.Instructions
import qualified Svvm.Utils.GraphvizDot as G
import Text.Printf

type MachineToIrPcMap = Map Int Int

data TranslationResult = TranslationResult
  { trDsl :: DslState,
    trIrCfg :: CFG,
    trMachineCfg :: CFG,
    trMachineToIrPc :: MachineToIrPcMap,
    trRegAlloc :: RegAllocState,
    trFinalCfg :: CFG
  }

translateProgram :: Dsl () -> TranslationResult
translateProgram dslProgram =
  TranslationResult
    { trDsl = dslState',
      trIrCfg = irCfg,
      trMachineCfg = machineCfg,
      trMachineToIrPc = machineToIrPc,
      trRegAlloc = regAlloc,
      trFinalCfg = finalCfg
    }
  where
    dslState = execState dslProgram initDslState
    irCfg = buildCfg dslState
    (loweredCfg, dslState') = flip runReader irCfg $ flip runStateT dslState $ mapM lowerBbInsts irCfg
    (machineToIrPc, machineCfg) = recomputeInstructionPcs loweredCfg
    regAlloc = assignPhysicalRegisters dslState' machineCfg
    finalCfg = renameRegisters dslState' regAlloc machineCfg

showProgramAssembly :: TranslationResult -> String
showProgramAssembly tr = showMachineInstsWithIr showInstructionAsm (trDsl tr) (trMachineToIrPc tr) (trFinalCfg tr)

showTranslationProcessGraph :: TranslationResult -> String
showTranslationProcessGraph tr =
  G.directedGraph
    "Translation"
    ( showDotCfgSubgraph (trIrCfg tr) "IR" "bb_ir"
        <> showDotCfgSubgraph (trMachineCfg tr) "Lowered" "bb_lowered"
        <> showDotCfgSubgraph (trFinalCfg tr) "Final" "bb_final"
        <> showDotRegAllocState (trDsl tr) (trRegAlloc tr)
    )

showMachineInstsWithIr :: (Instruction -> String) -> DslState -> MachineToIrPcMap -> CFG -> String
showMachineInstsWithIr showInst s pcMap = Seq.foldrWithIndex showBbInsts ""
  where
    showBbInsts bbIdx bb acc = "bb" <> show bbIdx <> ":\n" <> showInsts (bbInsts bb) (-1) <> acc
    showInsts Empty _ = ""
    showInsts ((pc, inst) :<| rest) lastIrPc
      | newIrPc <- pcMap Map.! pc,
        newIrPc /= lastIrPc =
        "// IR " <> show newIrPc <> ": " <> show (sPgm s `Seq.index` newIrPc) <> "\n" <> showInsts ((pc, inst) :<| rest) newIrPc
      | otherwise = showInst inst <> "\n" <> showInsts rest lastIrPc

showDotRegAllocState :: DslState -> RegAllocState -> String
showDotRegAllocState s rs =
  G.subgraph "RegAlloc" $
    G.node "stats" $
      G.table $
        G.tableRow (G.tableCol 3 $ G.textBold "Virtual SGPR liveness intervals:")
          <> showLiveness (sSgprBlocks s) "s" (rlSgprIntervals $ rsLiveness rs)
          <> G.tableRow (G.tableCol 3 $ G.textBold "Virtual SGPR mapping:")
          <> showMapping (rsSgprVirtToPhys rs) "s" (sSgprBlocks s)
          <> G.tableRow (G.tableCol 3 $ G.textBold "Virtual VGPR liveness intervals:")
          <> showLiveness (sVgprBlocks s) "v" (rlVgprIntervals $ rsLiveness rs)
          <> G.tableRow (G.tableCol 3 $ G.textBold "Virtual VGPR mapping:")
          <> showMapping (rsVgprVirtToPhys rs) "v" (sVgprBlocks s)
  where
    showLiveness rgroups rcls = Map.foldMapWithKey $ \rgFirst (LvInterval (start, end)) ->
      let group = rgroups Map.! rgFirst
          rgLast = rgFirst + rbSize group - 1
       in G.tableRow $ G.tableCol 1 ("%" <> showRegGroup rcls rgFirst rgLast) <> G.tableCol 2 (show (start, end))
    showMapping mapping rcls = Map.foldMapWithKey $ \rgFirst rg ->
      let rgLast = rgFirst + rbSize rg - 1
          physFirst = mapping Map.! rgFirst
          physLast = physFirst + rbSize rg - 1
       in G.tableRow $
            G.tableCol 1 ("%" <> showRegGroup rcls rgFirst rgLast)
              <> G.tableCol 1 ("alignment " <> show (rbAlignment rg))
              <> G.tableCol 1 (showRegGroup rcls physFirst physLast)
    showRegGroup rcls first last = rcls <> "[" <> show first <> ":" <> show last <> "]"

showDotCfgSubgraph :: CFG -> String -> String -> String
showDotCfgSubgraph cfg subgraphName bbPrefix = G.subgraph subgraphName $ mconcat $ basicBlock <$> zip [0 ..] (toList cfg)
  where
    basicBlock (bbIdx, bb) =
      let bbTitle = bbPrefix <> show bbIdx
          bbContents =
            G.node bbTitle $
              G.table $ G.tableRow (G.tableCol 2 (G.textBold bbTitle)) <> mconcat (instructionRow <$> toList (bbInsts bb))
          successorEdges = mconcat ((\succIdx -> G.edge bbTitle (bbPrefix <> show succIdx)) <$> bbSuccessors bb)
       in bbContents <> successorEdges
    instructionRow (pc, i) = G.tableRow (G.tableCol 1 (show pc) <> G.tableCol 1 (showInstructionDetailed i))

showInstructionAsm :: Instruction -> String
showInstructionAsm (INative opcode [] []) = opcode
showInstructionAsm (INative opcode operands mods) =
  opcode <> " " <> intercalate ", " (showOperand <$> operands) <> mconcat ((" " <>) <$> mods)
  where
    showOperand (NiLit l) = printf "0x%x" l
    showOperand (NiSymbol m) = m
    showOperand (NiVgprs RegLocPhys _ gprs) = "v" <> showRegIdxs gprs
    showOperand (NiSgprs RegLocPhys _ gprs) = "s" <> showRegIdxs gprs
    showOperand (NiVgprs RegLocVirt _ gprs) = error "Virtual register operands cannot be printed in an assembly instruction"
    showOperand (NiSgprs RegLocVirt _ gprs) = error "Virtual register operands cannot be printed in an assembly instruction"
    showRegIdxs [r] = show r
    showRegIdxs many = "[" <> show (head many) <> ":" <> show (last many) <> "]"
showInstructionAsm i = error $ "Intermediate instruction cannot be printed as an assembly instruction: " <> show i

showInstructionDetailed :: Instruction -> String
showInstructionDetailed (INative opcode [] []) = opcode
showInstructionDetailed (INative opcode operands mods) =
  opcode <> " " <> intercalate ", " (showOperand <$> operands) <> mconcat ((" " <>) <$> mods)
  where
    showOperand (NiLit l) = printf "0x%08x" l
    showOperand (NiSymbol m) = m
    showOperand (NiVgprs loc access gprs) = showRegAccess access <> showRegLoc loc <> "v" <> showRegIdxs gprs
    showOperand (NiSgprs loc access gprs) = showRegAccess access <> showRegLoc loc <> "s" <> showRegIdxs gprs
    showRegLoc RegLocVirt = "%"
    showRegLoc RegLocPhys = ""
    showRegAccess RegWr = "="
    showRegAccess RegRd = ""
    showRegIdxs [r] = show r
    showRegIdxs many = "[" <> show (head many) <> ":" <> show (last many) <> "]"
showInstructionDetailed i = show i
