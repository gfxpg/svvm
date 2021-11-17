{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Svvm.Instructions (module Svvm.Instructions, module Svvm.Storage) where

import Data.Word (Word32)
import Svvm.Storage
import Svvm.Utils.TypeUnion

newtype Label = Label Int {- index into a sequence of label targets -} deriving (Eq, Show)

newtype LabelTarget = LabelTarget Int {- index into a sequence of instructions -} deriving (Eq, Show)

-- Operations

data CmpLt = CmpLt deriving (Eq, Show)

data CmpEq = CmpEq deriving (Eq, Show)

data ArithAdd = ArithAdd deriving (Eq, Show)

data ArithSub = ArithSub deriving (Eq, Show)

data ArithMul = ArithMul deriving (Eq, Show)

data BitwAnd = BitwAnd deriving (Eq, Show)

data BitwOr = BitwOr deriving (Eq, Show)

data BitwShl = BitwShl deriving (Eq, Show)

type IsCmpOp t = IsMember t '[CmpLt, CmpEq]

type IsArithOp t = IsMember t '[ArithAdd, ArithSub, ArithMul]

type IsBitwiseOp t = IsMember t '[BitwAnd, BitwOr, BitwShl]

type ScalarSrc t n = TypeUnion '[SRegs t n, Int, Float]

type ScalarDst t n = SRegs t n

type VectorSrc t n = TypeUnion '[VRegs t n, SRegs t n, Int, Float]

type VectorDst t n = VRegs t n

type Offset = Int

data BranchCond = BrScc | BrNotScc | BrVcc | BrNotVcc
  deriving (Eq, Show)

data NativeInstOperand
  = NiLit Word32
  | NiSgprs RegLocation RegAccess [Int]
  | NiVgprs RegLocation RegAccess [Int]
  | NiSymbol String
  deriving (Eq, Show)

data RegAccess = RegRd | RegWr
  deriving (Eq, Show)

data Instruction where
  IVectorMoveN :: IsStorable t n => VectorDst t n -> VectorSrc t n -> Instruction
  IScalarMoveN :: IsStorable t n => ScalarDst t n -> ScalarSrc t n -> Instruction
  IBranch :: Label -> Instruction
  ICondBranch :: BranchCond -> Label -> Instruction
  IEndProgram :: Instruction
  IScalarLoadN :: IsStorable t n => ScalarDst t n -> SRegs U32 2 -> Offset -> Instruction
  IBufferLoadN :: IsStorable t n => VectorDst t n -> VReg U32 -> SRegs U32 4 -> Offset -> Instruction
  IBufferStoreN :: IsStorable t n => VRegs t n -> VReg U32 -> SRegs U32 4 -> Offset -> Instruction
  IScalarArithN :: (IsStorable t n, IsArithOp o) => Ty o -> ScalarDst t n -> ScalarSrc t n -> ScalarSrc t n -> Instruction
  IVectorArithN :: (IsStorable t n, IsArithOp o) => Ty o -> VectorDst t n -> VectorSrc t n -> VectorSrc t n -> Instruction
  IScalarBitwiseN :: (IsStorable t n, IsBitwiseOp o) => Ty o -> ScalarDst t n -> ScalarSrc t n -> ScalarSrc t n -> Instruction
  IVectorBitwiseN :: (IsStorable t n, IsBitwiseOp o) => Ty o -> VectorDst t n -> VectorSrc t n -> VectorSrc t n -> Instruction
  IScalarCmp :: (IsStorable t 1, IsCmpOp o) => Ty o -> ScalarSrc t 1 -> ScalarSrc t 1 -> Instruction
  IVectorCmp :: (IsStorable t 1, IsCmpOp o) => Ty o -> VectorSrc t 1 -> VectorSrc t 1 -> Instruction
  -- opcode -> operands -> modifiers
  INative :: String -> [NativeInstOperand] -> [String] -> Instruction

deriving instance Show Instruction

instance Eq Instruction where
  -- HACK: Eq cannot be automatically derived due to Proxies and KnownNats
  -- (how do you evaluate (KnownNat n1, KnownNat n2) => Instance n1 -> Instance n2 -> Bool?)
  -- Writing instances by hand gets tiring and error prone really fast
  a == b = show a == show b
