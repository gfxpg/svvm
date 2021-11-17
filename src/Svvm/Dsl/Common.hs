{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Svvm.Dsl.Common where

import Control.Monad.Trans.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits
import Svvm.Instructions
import Svvm.Utils.TypeUnion
import Prelude hiding (fromInteger, (!!))
import qualified Prelude

-- RebindableSyntax: integer literals are desugared to (fromInteger lit)
fromInteger :: Integer -> Int
fromInteger = Prelude.fromIntegral

fromRational :: Rational -> Float
fromRational = Prelude.fromRational

-- RebindableSyntax: if cond then x else y is desugared to (ifThenElse cond x y)
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y

data RegBlock = RegBlock {rbSize :: Int, rbAlignment :: Int}

data DslState = DslState
  { sNumSgprs :: Int,
    sNumVgprs :: Int,
    sSgprBlocks :: Map Int RegBlock, -- key is the block start index (e.g. 0, 4, 5 for alloc :: SRegs 4 >> alloc :: SReg >> alloc :: SRegs 2)
    sVgprBlocks :: Map Int RegBlock,
    sPhysSgprs :: Set Int, -- HACK: register allocator should track the liveness of physical registers as well; for now we assume they're always live
    sPhysVgprs :: Set Int,
    sPgm :: Seq Instruction,
    sLabelTargets :: Seq (Maybe LabelTarget)
  }

initDslState :: DslState
initDslState =
  DslState
    { sNumSgprs = 0,
      sNumVgprs = 0,
      sSgprBlocks = mempty,
      sVgprBlocks = mempty,
      sPhysSgprs = mempty,
      sPhysVgprs = mempty,
      sPgm = mempty,
      sLabelTargets = mempty
    }

type Dsl = State DslState

emit :: Instruction -> Dsl ()
emit i = modify $ \s -> s {sPgm = sPgm s |> i}

data ScalarExpr t n
  = IsStorable t n => ScalarExpr (ScalarDst t n -> Dsl ())
  | IsStorable t n => ScalarData (ScalarSrc t n)

class IsStorable t n => LiftScalarExpr a t n where
  liftScalar :: a -> ScalarExpr t n

instance IsStorable t n => LiftScalarExpr (ScalarExpr t n) t n where
  liftScalar = id

instance IsStorable t n => LiftScalarExpr (SRegs t n) t n where
  liftScalar srcregs = ScalarData (liftU srcregs)

instance (IsInteger t, IsStorable t n) => LiftScalarExpr Int t n where
  liftScalar = ScalarData . liftU

instance (IsFloatingPoint t, IsStorable t n) => LiftScalarExpr Float t n where
  liftScalar = ScalarData . liftU

data VectorExpr t n
  = IsStorable t n => VectorExpr (VectorDst t n -> Dsl ())
  | IsStorable t n => VectorData (VectorSrc t n)

class IsStorable t n => LiftVectorExpr a t n where
  liftVector :: a -> VectorExpr t n

instance IsStorable t n => LiftVectorExpr (VectorExpr t n) t n where
  liftVector = id

instance IsStorable t n => LiftVectorExpr (ScalarExpr t n) t n where
  liftVector (ScalarExpr emitS) = VectorExpr $ \dst -> do
    tmpreg :: SRegs t n <- alloc
    emitS tmpreg
    emit $ IVectorMoveN dst (liftU tmpreg)
  liftVector (ScalarData constS) = VectorData $ (liftU @> liftU @> liftU @> unionExhausted) constS

instance IsStorable t n => LiftVectorExpr (SRegs t n) t n where
  liftVector srcregs = VectorData (liftU srcregs)

instance IsStorable t n => LiftVectorExpr (VRegs t n) t n where
  liftVector srcregs = VectorData (liftU srcregs)

instance (IsInteger t, IsStorable t n) => LiftVectorExpr Int t n where
  liftVector = VectorData . liftU

instance (IsFloatingPoint t, IsStorable t n) => LiftVectorExpr Float t n where
  liftVector = VectorData . liftU

-- Type casts (no data conversion)

class CastOps t' t reg where
  cast :: (IsStorable t' n, IsStorable t n) => reg t n -> reg t' n

instance CastOps t' t SRegs where
  cast (SRegs t n loc i) = SRegs (ty @t') n loc i

instance CastOps t' t VRegs where
  cast (VRegs t n loc i) = VRegs (ty @t') n loc i

-- Register allocation

class AllocOp st where
  alloc :: Dsl st
  allocPhysical :: Int -> Dsl st

instance IsStorable t n => AllocOp (VRegs t n) where
  alloc = state $ \s -> do
    let gprsRequested = tyInt (ty @n)
        nextFreeGpr = sNumVgprs s
        reg = VRegs (ty @t) (ty @n) nextFreeGpr RegLocVirt
        s' =
          s
            { sNumVgprs = nextFreeGpr + gprsRequested,
              sVgprBlocks =
                Map.insert nextFreeGpr (RegBlock {rbSize = gprsRequested, rbAlignment = 1}) (sVgprBlocks s)
            }
     in (reg, s')
  allocPhysical fromVgpr = state $ \s -> do
    let gprsRequested = tyInt (ty @n)
        s' = s {sPhysVgprs = foldr Set.insert (sPhysVgprs s) [fromVgpr .. fromVgpr + gprsRequested - 1]}
        reg = VRegs (ty @t) (ty @n) fromVgpr RegLocPhys
     in (reg, s')

instance IsStorable t n => AllocOp (SRegs t n) where
  alloc = state $ \s -> do
    let gprsRequested = tyInt (ty @n)
        nextFreeGpr = sNumSgprs s
        reg = SRegs (ty @t) (ty @n) nextFreeGpr RegLocVirt
        s' =
          s
            { sNumSgprs = nextFreeGpr + gprsRequested,
              sSgprBlocks =
                Map.insert nextFreeGpr (RegBlock {rbSize = gprsRequested, rbAlignment = 1}) (sSgprBlocks s)
            }
     in (reg, s')
  allocPhysical fromSgpr = state $ \s -> do
    let gprsRequested = tyInt (ty @n)
        s' = s {sPhysSgprs = foldr Set.insert (sPhysSgprs s) [fromSgpr .. fromSgpr + gprsRequested - 1]}
        reg = SRegs (ty @t) (ty @n) fromSgpr RegLocPhys
     in (reg, s')

-- Assignment

infixr 1 .=

class AsgnOp st v where
  (.=) :: st -> v -> Dsl ()

instance (IsStorable t n, LiftScalarExpr e t n) => AsgnOp (SRegs t n) e where
  dstregs .= src = case liftScalar src :: ScalarExpr t n of
    ScalarExpr e -> e dstregs
    ScalarData src -> emit $ IScalarMoveN dstregs src

instance (IsStorable t n, LiftVectorExpr e t n) => AsgnOp (VRegs t n) e where
  dstregs .= src = case liftVector src :: VectorExpr t n of
    VectorExpr e -> e dstregs
    VectorData src -> emit $ IVectorMoveN dstregs src

-- Memory operations

{-# ANN s_load_dwordN "HLint: ignore Use camelCase" #-}
s_load_dwordN :: (IsStorable t n) => SRegs t n -> SRegs U32 2 -> Offset -> Dsl ()
s_load_dwordN dst addr offset = emit $ IScalarLoadN dst addr offset

{-# ANN buffer_load_dwordN "HLint: ignore Use camelCase" #-}
buffer_load_dwordN :: (IsStorable t n) => VRegs t n -> VReg U32 -> SRegs U32 4 -> Offset -> Dsl ()
buffer_load_dwordN dst off srd offset = emit $ IBufferLoadN dst off srd offset

{-# ANN buffer_store_dwordN "HLint: ignore Use camelCase" #-}
buffer_store_dwordN :: (IsStorable t n) => VRegs t n -> VReg U32 -> SRegs U32 4 -> Offset -> Dsl ()
buffer_store_dwordN dst off srd offset = emit $ IBufferStoreN dst off srd offset

-- HACK: this is the best we can do until automated waitcnt insertion is implemented in the backend
{-# ANN s_waitcnt_0 "HLint: ignore Use camelCase" #-}
s_waitcnt_0 :: Dsl ()
s_waitcnt_0 = emit $ INative "s_waitcnt" [NiLit 0] []

-- Subregister indexing (?)

class (IsStorable t newlen, KnownNat from) => RegSliceOp newlen from regs t where
  slice :: regs t n -> regs t newlen

instance (IsStorable t newlen, KnownNat from) => RegSliceOp newlen from SRegs t where
  slice (SRegs t _ start loc) = SRegs t (ty @newlen) (start + tyInt (ty @from)) loc

instance (IsStorable t newlen, KnownNat from) => RegSliceOp newlen from VRegs t where
  slice (VRegs t _ start loc) = VRegs t (ty @newlen) (start + tyInt (ty @from)) loc

class IndexingOps ts t | ts -> t where
  (!!) :: ts -> Int -> t

instance IndexingOps [a] a where
  (!!) = (Prelude.!!)

instance (IsStorable t n, IsStorable t 1) => IndexingOps (SRegs t n) (SReg t) where
  (SRegs t n s1 loc) !! i
    | i < tyInt n = SRegs t (ty @1) (s1 + i) loc
    | otherwise = error $ "!!: register " <> show i <> " out of 0:" <> show (tyInt n) <> " requested"

instance (IsStorable t n, IsStorable t 1) => IndexingOps (VRegs t n) (VReg t) where
  (VRegs t n s1 loc) !! i
    | i < tyInt n = VRegs t (ty @1) (s1 + i) loc
    | otherwise = error $ "!!: register " <> show i <> " out of 0:" <> show (tyInt n) <> " requested"
