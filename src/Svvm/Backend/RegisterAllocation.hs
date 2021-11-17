{-# LANGUAGE NamedFieldPuns #-}

module Svvm.Backend.RegisterAllocation where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Svvm.Backend.Common
import Svvm.Dsl hiding (CompOp (..), ElementwiseOp (..))
import Svvm.Instructions

maxSgprs :: Int
maxSgprs = 106

maxVgprs :: Int
maxVgprs = 255

data RegAllocState = RegAllocState
  { rsLiveness :: RegisterLiveness,
    rsSgprVirtToPhys :: Map Int Int,
    rsVgprVirtToPhys :: Map Int Int
  }

assignPhysicalRegisters :: DslState -> CFG -> RegAllocState
assignPhysicalRegisters s cfg =
  RegAllocState {rsLiveness = liveness, rsSgprVirtToPhys = sgprMapping, rsVgprVirtToPhys = vgprMapping}
  where
    liveness = analyzeLiveness s cfg
    sgprMapping = pickRegisters maxSgprs (sPhysSgprs s) (sSgprBlocks s) (rlSgprIntervals liveness)
    vgprMapping = pickRegisters maxVgprs (sPhysVgprs s) (sVgprBlocks s) (rlVgprIntervals liveness)

renameRegisters :: DslState -> RegAllocState -> CFG -> CFG
renameRegisters s rs cfg = (\bb -> bb {bbInsts = renameInstOperands <$> bbInsts bb}) <$> cfg
  where
    renameInstOperands (pc, INative opcode operands mods) = (pc, INative opcode (renameOperand <$> operands) mods)
    renameInstOperands i = error $ "Unsupported instruction " <> show i
    renameOperand (NiSgprs RegLocVirt access sgprs) =
      let Just (virtRgStart, _) = Map.lookupLE (head sgprs) (sSgprBlocks s)
          physGpr = rsSgprVirtToPhys rs Map.! virtRgStart + (head sgprs - virtRgStart)
       in NiSgprs RegLocPhys access [physGpr .. physGpr + length sgprs - 1]
    renameOperand (NiVgprs RegLocVirt access vgprs) =
      let Just (virtRgStart, _) = Map.lookupLE (head vgprs) (sVgprBlocks s)
          physGpr = rsVgprVirtToPhys rs Map.! virtRgStart + (head vgprs - virtRgStart)
       in NiVgprs RegLocPhys access [physGpr .. physGpr + length vgprs - 1]
    renameOperand o = o

pickRegisters :: Int -> Set Int -> Map Int RegBlock -> Map Int LvInterval -> Map Int Int
pickRegisters numGprs physRegs virtRegBlocks liveness =
  go lvIntervalsAscStart Map.empty ([], initFreeRegs)
  where
    initFreeRegs = Set.foldr (\physReg freeRegs -> let (ls, _ : rs) = splitAt physReg freeRegs in ls ++ [False] ++ rs) (replicate numGprs True) physRegs
    lvIntervals = Map.toAscList liveness
    lvIntervalsAscStart = sortOn (\(_, LvInterval (start, _)) -> start) lvIntervals
    go [] regMap _ = regMap
    go (int@(vregStart, LvInterval (start, end)) : rest) regMap (activeInts, freeRegs) =
      let (activeInts', freeRegs') = expireOld int activeInts regMap freeRegs
          rgAllocated = virtRegBlocks Map.! vregStart
          physRegStart = findFreeRegs (rbSize rgAllocated) (rbAlignment rgAllocated) 0 freeRegs'
          freeRegs'' = take physRegStart freeRegs ++ replicate (rbSize rgAllocated) False ++ drop (physRegStart + rbSize rgAllocated) freeRegs
          activeInts'' = sortOn (\(_, LvInterval (_, end)) -> end) (int : activeInts)
          regMap' = Map.insert vregStart physRegStart regMap
       in go rest regMap' (activeInts'', freeRegs'')
    expireOld _ [] _ freeRegs = ([], freeRegs)
    expireOld currInt@(_, LvInterval (currStart, _)) activeInts@((virtActiveRg, LvInterval (_, activeEnd)) : restInts) regMap freeRegs
      | currStart > activeEnd =
        let expiredPhysStart = regMap Map.! virtActiveRg
            expiredRg = virtRegBlocks Map.! virtActiveRg
            freeRegs' = take expiredPhysStart freeRegs ++ replicate (rbSize expiredRg) True ++ drop (expiredPhysStart + rbSize expiredRg) freeRegs
         in expireOld currInt restInts regMap freeRegs'
      | otherwise = (activeInts, freeRegs)
    findFreeRegs _ _ _ [] =
      error "No free registers"
    findFreeRegs s_size alignment i freeRegs
      | and (take s_size freeRegs) = i
      | otherwise = findFreeRegs s_size alignment (i + alignment) (drop alignment freeRegs)

-- Liveness analysis

newtype LvInterval = LvInterval (Int, Int)
  deriving (Eq, Show)

mergeLvInts :: LvInterval -> LvInterval -> LvInterval
mergeLvInts (LvInterval (lb, ub)) (LvInterval (lb', ub')) = LvInterval (min lb lb', max ub ub')

data RegisterLiveness = RegisterLiveness
  { rlSgprIntervals :: Map Int LvInterval,
    rlSgprLiveOuts :: Map Int (Set Int), -- not used ATM, intended for read before write checks
    rlVgprIntervals :: Map Int LvInterval,
    rlVgprLiveOuts :: Map Int (Set Int)
  }
  deriving (Eq, Show)

analyzeLiveness :: DslState -> CFG -> RegisterLiveness
analyzeLiveness s cfg = RegisterLiveness {rlSgprIntervals, rlSgprLiveOuts, rlVgprIntervals, rlVgprLiveOuts}
  where
    (rlSgprIntervals, rlSgprLiveOuts) = regClassLiveness selectSgprOperands cfg
    selectSgprOperands (NiSgprs RegLocVirt access sgprs) =
      let Just (groupStart, _) = Map.lookupLE (minimum sgprs) (sSgprBlocks s)
       in Just (access, groupStart)
    selectSgprOperands _ = Nothing
    (rlVgprIntervals, rlVgprLiveOuts) = regClassLiveness selectVgprOperands cfg
    selectVgprOperands (NiVgprs RegLocVirt access vgprs) =
      let Just (groupStart, _) = Map.lookupLE (minimum vgprs) (sVgprBlocks s)
       in Just (access, groupStart)
    selectVgprOperands _ = Nothing

type RegGroupIdx = Int

regClassLiveness :: (NativeInstOperand -> Maybe (RegAccess, RegGroupIdx)) -> CFG -> (Map Int LvInterval, Map Int (Set Int))
regClassLiveness selectRegOperand cfg = (lvMap, liveOuts)
  where
    (lvMap, bbIns) = overBbs cfg (Map.empty, Seq.empty)
    liveOuts = updateUntilConvergence $ Map.fromDistinctAscList $ zip [0 .. Seq.length cfg - 1] (repeat Set.empty)
      where
        updateUntilConvergence liveOutMap =
          let liveOutMap' = foldl' updateLiveOut liveOutMap (Map.keys liveOutMap)
           in if liveOutMap == liveOutMap'
                then liveOutMap
                else updateUntilConvergence liveOutMap'
        updateLiveOut liveOutMap bbIdx =
          let bbLiveOut = mconcat $ computeSuccLiveIns <$> bbSuccessors (cfg `Seq.index` bbIdx)
              computeSuccLiveIns succIdx = (bbIns `Seq.index` succIdx) `Set.union` (liveOutMap Map.! succIdx)
           in Map.insertWith Set.union bbIdx bbLiveOut liveOutMap
    overBbs Empty acc = acc
    overBbs (bb :<| rest) (lvMap, bbIns) =
      let (lvMap', bbIn) = overInsts (bbInsts bb) (lvMap, Set.empty)
       in overBbs rest (lvMap', bbIns |> bbIn)
    overInsts Empty acc = acc
    overInsts (rest :|> (pc, INative _ operands _)) (lvMap, bbIn) =
      let (lvMap', bbIn') = overOperands pc operands (lvMap, bbIn)
       in overInsts rest (lvMap', bbIn')
    overInsts (_ :|> i) _ = error $ "Unsupported instruction " <> show i
    overOperands _ [] acc = acc
    overOperands pc (op : rest) (lvMap, bbIn)
      | Just (access, groupStart) <- selectRegOperand op =
        let lvMap' = Map.insertWith mergeLvInts groupStart (LvInterval (pc, pc)) lvMap
            -- We need to know whether this register group is live across blocks:
            bbIn' = case access of
              -- If the current instruction reads it, then some preceding instruction (possibly in another block) has written it
              RegRd -> Set.insert groupStart bbIn
              -- If the current instruction overwrites it, then previous reads refer to the value set in this block
              -- (remember that we're visiting the instructions in reverse)
              RegWr -> Set.delete groupStart bbIn
         in overOperands pc rest (lvMap', bbIn')
      | otherwise = overOperands pc rest (lvMap, bbIn)
