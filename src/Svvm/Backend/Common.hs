module Svvm.Backend.Common where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Svvm.Instructions

type BasicBlockIdx = Int

data BasicBlock = BasicBlock {bbInsts :: Seq (Int, Instruction), bbSuccessors :: [Int], bbPredecessors :: [Int]}
  deriving (Eq, Show)

type CFG = Seq BasicBlock

recomputeInstructionPcs :: CFG -> (Map Int Int, CFG) -- new pc -> old pc
recomputeInstructionPcs cfg = (pcMap, newBbs)
  where
    (_, pcMap, newBbs) = overBbs 0 Map.empty cfg
    overBbs pc pcMap Empty = (pc, pcMap, Empty)
    overBbs pc pcMap (bb :<| rest) =
      let (pc', pcMap', bbInsts') = overInsts pc pcMap (bbInsts bb)
          (pc'', pcMap'', rest') = overBbs pc' pcMap' rest
       in (pc'', pcMap'', bb {bbInsts = bbInsts'} <| rest')
    overInsts pc pcMap Empty = (pc, pcMap, Empty)
    overInsts pc pcMap ((oldPc, inst) :<| rest) =
      let (pc', pcMap', rest') = overInsts (pc + 1) (Map.insert pc oldPc pcMap) rest
       in (pc', pcMap', (pc, inst) <| rest')
