{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Svvm.Dsl.ControlFlow where

import Control.Monad.Trans.State
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Svvm.Dsl.Common
import Svvm.Instructions
import Svvm.Utils.TypeUnion

emptyLabel :: Dsl Label
emptyLabel = do
  label <- gets $ Label . Seq.length . sLabelTargets
  modify $ \s -> s {sLabelTargets = sLabelTargets s |> Nothing}
  pure label

placeLabel :: Label -> Dsl ()
placeLabel (Label labelIdx) = modify $ \s ->
  let target = LabelTarget $ Seq.length $ sPgm s
   in s {sLabelTargets = Seq.update labelIdx (Just target) (sLabelTargets s)}

data ScalarPredicate = ScalarPredicate BranchCond (Dsl ())

invertBrCond :: BranchCond -> BranchCond
invertBrCond c = case c of BrScc -> BrNotScc; BrVcc -> BrNotVcc; BrNotScc -> BrScc; BrNotVcc -> BrVcc

infixl 4 <

class CompOp a b c | a b -> c where
  (<) :: a -> b -> c

instance CompOp Int Int Bool where
  (<) = (Prelude.<)

instance (IsStorable t n, n ~ 1) => CompOp (SReg t) Int ScalarPredicate where
  reg < i = ScalarPredicate BrScc $ emit $ IScalarCmp (ty :: Ty CmpLt) (liftU reg :: ScalarSrc t 1) (liftU i :: ScalarSrc t 1)

instance (IsStorable t n, n ~ 1) => CompOp (VReg t) (SReg t) ScalarPredicate where
  vreg < sreg = ScalarPredicate BrVcc $ emit $ IVectorCmp (ty :: Ty CmpLt) (liftU vreg :: VectorSrc t 1) (liftU sreg :: VectorSrc t 1)

class UnaryBoolOp a where
  not :: a -> a

instance UnaryBoolOp Bool where
  not = Prelude.not

instance UnaryBoolOp ScalarPredicate where
  not (ScalarPredicate cond setCond) = ScalarPredicate (invertBrCond cond) setCond

infixl 3 &&

infixl 2 ||

class BinaryBoolOp a b c | a b -> c where
  (&&) :: a -> b -> c
  (||) :: a -> b -> c

instance BinaryBoolOp Bool Bool Bool where
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)

branchIf :: ScalarPredicate -> Label -> Dsl ()
branchIf (ScalarPredicate cond setCond) label = setCond >> emit (ICondBranch cond label)

branchIfNot :: ScalarPredicate -> Label -> Dsl ()
branchIfNot (ScalarPredicate cond setCond) label = setCond >> emit (ICondBranch (invertBrCond cond) label)

instance BinaryBoolOp ScalarPredicate ScalarPredicate ScalarPredicate where
  predicate1 && (ScalarPredicate cond2 setCond2) =
    ScalarPredicate cond2 $ emptyLabel >>= \labelFalse -> branchIfNot predicate1 labelFalse >> setCond2 >> placeLabel labelFalse
  predicate1 || (ScalarPredicate cond2 setCond2) =
    ScalarPredicate cond2 $ emptyLabel >>= \labelTrue -> branchIf predicate1 labelTrue >> setCond2 >> placeLabel labelTrue

if_ :: ScalarPredicate -> Dsl () -> Dsl ()
if_ predicate body =
  emptyLabel >>= \labelEnd -> branchIfNot predicate labelEnd >> body >> placeLabel labelEnd

while_ :: ScalarPredicate -> Dsl () -> Dsl ()
while_ predicate body = do
  labelLoop <- emptyLabel
  placeLabel labelLoop
  if_ predicate $ do
    body
    emit $ IBranch labelLoop

endpgm :: Dsl ()
endpgm = emit IEndProgram

-- infixl 0 ?
-- infixl 1 :?
-- data CondSelect a b = a :? b
-- class CondSelectOp a b c d | a b c -> d where
--   (?) :: a -> CondSelect b c -> d
-- instance CondSelectOp Bool a a a where
--   True ? (a :? _) = a
--   False ? (_ :? b) = b
-- instance CondSelectOp ScalarPredicate ()
