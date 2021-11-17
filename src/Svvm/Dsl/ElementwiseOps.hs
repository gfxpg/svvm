{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Svvm.Dsl.ElementwiseOps where

import Data.Bits
import Data.Type.Equality (type (==))
import GHC.TypeLits
import Svvm.Dsl.Common
import Svvm.Instructions
import Svvm.Utils.TypeUnion
import Prelude hiding (Num (..), fromInteger)
import qualified Prelude

infixl 4 <<

infixl 6 +, -

infixl 7 &, *

class ElementwiseOp x y r | x y -> r where
  (+) :: x -> y -> r
  (-) :: x -> y -> r
  (*) :: x -> y -> r
  (&) :: x -> y -> r
  (<<) :: x -> y -> r

instance ElementwiseOp Int Int Int where
  (+) = (Prelude.+)
  (-) = (Prelude.-)
  (*) = (Prelude.*)
  (&) = (.&.)
  (<<) = shiftL

instance ElementwiseOp Float Float Float where
  (+) = (Prelude.+)
  (-) = (Prelude.-)
  (*) = (Prelude.*)

  -- TODO: move bitwise operations into a separate typeclass so this becomes a type error
  (&) = error "Bitwise operations are not implemented for floats"
  (<<) = error "Bitwise operations are not implemented for floats"

instance (IsStorable t n) => ElementwiseOp (ScalarExpr t n) (ScalarExpr t n) (ScalarExpr t n) where
  a + b = scalarInstExpr a b $ IScalarArithN (ty :: Ty ArithAdd)
  a - b = scalarInstExpr a b $ IScalarArithN (ty :: Ty ArithSub)
  a * b = scalarInstExpr a b $ IScalarArithN (ty :: Ty ArithMul)
  a & b = scalarInstExpr a b $ IScalarBitwiseN (ty :: Ty BitwAnd)
  a << b = scalarInstExpr a b $ IScalarBitwiseN (ty :: Ty BitwShl)

instance (IsStorable t n) => ElementwiseOp (VectorExpr t n) (VectorExpr t n) (VectorExpr t n) where
  a + b = vectorInstExpr a b $ IVectorArithN (ty :: Ty ArithAdd)
  a - b = vectorInstExpr a b $ IVectorArithN (ty :: Ty ArithSub)
  a * b = vectorInstExpr a b $ IVectorArithN (ty :: Ty ArithMul)
  a & b = vectorInstExpr a b $ IVectorBitwiseN (ty :: Ty BitwAnd)
  a << b = vectorInstExpr a b $ IVectorBitwiseN (ty :: Ty BitwShl)

type ScalarBinInstCons t n = ScalarDst t n -> ScalarSrc t n -> ScalarSrc t n -> Instruction

scalarInstExpr :: (IsStorable t n) => ScalarExpr t n -> ScalarExpr t n -> ScalarBinInstCons t n -> ScalarExpr t n
scalarInstExpr (ScalarExpr emitA :: ScalarExpr t n) (ScalarExpr emitB :: ScalarExpr t n) inst = ScalarExpr $ \dst -> do
  tmpreg :: SRegs t n <- alloc
  emitA dst
  emitB tmpreg
  emit (inst dst (liftU dst) (liftU tmpreg))
scalarInstExpr (ScalarExpr emitA :: ScalarExpr t n) (ScalarData constB) inst = ScalarExpr $ \dst ->
  emitA dst >> emit (inst dst (liftU dst) constB)
scalarInstExpr (ScalarData constA) (ScalarExpr emitB :: ScalarExpr t n) inst = ScalarExpr $ \dst ->
  emitB dst >> emit (inst dst constA (liftU dst))
scalarInstExpr (ScalarData constA) (ScalarData constB) inst = ScalarExpr $ \dst ->
  emit (inst dst constA constB)

type VectorBinInstConst t n = VectorDst t n -> VectorSrc t n -> VectorSrc t n -> Instruction

vectorInstExpr :: (IsStorable t n) => VectorExpr t n -> VectorExpr t n -> VectorBinInstConst t n -> VectorExpr t n
vectorInstExpr (VectorExpr emitA :: VectorExpr t n) (VectorExpr emitB :: VectorExpr t n) inst = VectorExpr $ \dst -> do
  tmpreg :: VRegs t n <- alloc
  emitA dst
  emitB tmpreg
  emit (inst dst (liftU dst) (liftU tmpreg))
vectorInstExpr (VectorExpr emitA :: VectorExpr t n) (VectorData constB) inst = VectorExpr $ \dst ->
  emitA dst >> emit (inst dst (liftU dst) constB)
vectorInstExpr (VectorData constA) (VectorExpr emitB :: VectorExpr t n) inst = VectorExpr $ \dst ->
  emitB dst >> emit (inst dst constA (liftU dst))
vectorInstExpr (VectorData constA) (VectorData constB) inst = VectorExpr $ \dst ->
  emit (inst dst constA constB)

-- Boilerplate for implicitly lifting operands into ScalarExpr/VectorExpr

-- Note: LiftVectorExpr and LiftScalarExpr are manually inlined below.
-- This is done to prevent conflicts in functional dependencies and overlapping instances.
-- Hopefully this boilerplate can be reduced later with some type-level magic or, failing that, Template Haskell.

{- Use the script below to generate instances:

scalar_expr = "(ScalarExpr t n)"
vector_expr = "(VectorExpr t n)"

scalar_srcs = ["(SRegs t n)", scalar_expr]
vector_srcs = ["(VRegs t n)", vector_expr]
scalar_vector_srcs = scalar_srcs + vector_srcs

ops = ["+", "-", "*", "&", "<<"]

for src0 in scalar_srcs:
    for src1 in scalar_srcs:
        if src0 == scalar_expr and src1 == scalar_expr:
            continue # defined by hand

        print(f"instance (IsStorable t n) => ElementwiseOp {src0} {src1} {scalar_expr} where")

        for op in ops:
            print(f"  a {op} b = (liftScalar a :: {scalar_expr}) {op} (liftScalar b :: {scalar_expr})")

for src0 in scalar_vector_srcs:
    for src1 in scalar_vector_srcs:
        if src0 in scalar_srcs and src1 in scalar_srcs:
            continue
        if src0 == vector_expr and src1 == vector_expr:
            continue # defined by hand

        print(f"instance (IsStorable t n) => ElementwiseOp {src0} {src1} {vector_expr} where")

        for op in ops:
            print(f"  a {op} b = (liftVector a :: {vector_expr}) {op} (liftVector b :: {vector_expr})")

for src in scalar_srcs:
    print(f"instance (IsInteger t, IsStorable t n) => ElementwiseOp Int {src} {scalar_expr} where")
    for op in ops:
        print(f"  a {op} b = (liftScalar a :: {scalar_expr}) {op} (liftScalar b :: {scalar_expr})")

    print(f"instance (IsInteger t, IsStorable t n) => ElementwiseOp {src} Int {scalar_expr} where")
    for op in ops:
        print(f"  a {op} b = (liftScalar a :: {scalar_expr}) {op} (liftScalar b :: {scalar_expr})")

for src in scalar_srcs:
    print(f"instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp Float {src} {scalar_expr} where")
    for op in ops:
        print(f"  a {op} b = (liftScalar a :: {scalar_expr}) {op} (liftScalar b :: {scalar_expr})")

    print(f"instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp {src} Float {scalar_expr} where")
    for op in ops:
        print(f"  a {op} b = (liftScalar a :: {scalar_expr}) {op} (liftScalar b :: {scalar_expr})")

for src in vector_srcs:
    print(f"instance (IsInteger t, IsStorable t n) => ElementwiseOp Int {src} {vector_expr} where")
    for op in ops:
        print(f"  a {op} b = (liftVector a :: {vector_expr}) {op} (liftVector b :: {vector_expr})")

    print(f"instance (IsInteger t, IsStorable t n) => ElementwiseOp {src} Int {vector_expr} where")
    for op in ops:
        print(f"  a {op} b = (liftVector a :: {vector_expr}) {op} (liftVector b :: {vector_expr})")

for src in vector_srcs:
    print(f"instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp Float {src} {vector_expr} where")
    for op in ops:
      print(f"  a {op} b = (liftVector a :: {vector_expr}) {op} (liftVector b :: {vector_expr})")

    print(f"instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp {src} Float {vector_expr} where")
    for op in ops:
        print(f"  a {op} b = (liftVector a :: {vector_expr}) {op} (liftVector b :: {vector_expr})")

# Helpful type errors:

for src0 in scalar_srcs:
    for src1 in scalar_srcs:
        src1_mismatched_type = src1.replace(" t n", " u n")

        print("instance {-# OVERLAPPABLE #-} ")
        print(f"""  (TypeError (
        Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
              :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
          ),
        (t == u) ~ False
      ) =>
      ElementwiseOp {src0} {src1_mismatched_type} {scalar_expr} where
      """)

        for op in ops:
            print(f"  a {op} b = undefined")

for src0 in scalar_vector_srcs:
    for src1 in scalar_vector_srcs:
        if src0 in scalar_srcs and src1 in scalar_srcs:
            continue

        src1_mismatched_type = src1.replace(" t n", " u n")

        print("instance {-# OVERLAPPABLE #-} ")
        print(f"""  (TypeError (
        Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
              :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
          ),
        (t == u) ~ False
      ) =>
      ElementwiseOp {src0} {src1_mismatched_type} {vector_expr} where
      """)

        for op in ops:
            print(f"  a {op} b = undefined")

-}

instance (IsStorable t n) => ElementwiseOp (SRegs t n) (SRegs t n) (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsStorable t n) => ElementwiseOp (SRegs t n) (ScalarExpr t n) (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsStorable t n) => ElementwiseOp (ScalarExpr t n) (SRegs t n) (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsStorable t n) => ElementwiseOp (SRegs t n) (VRegs t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (SRegs t n) (VectorExpr t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (ScalarExpr t n) (VRegs t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (ScalarExpr t n) (VectorExpr t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (VRegs t n) (SRegs t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (VRegs t n) (ScalarExpr t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (VRegs t n) (VRegs t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (VRegs t n) (VectorExpr t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (VectorExpr t n) (SRegs t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (VectorExpr t n) (ScalarExpr t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsStorable t n) => ElementwiseOp (VectorExpr t n) (VRegs t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsInteger t, IsStorable t n) => ElementwiseOp Int (SRegs t n) (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsInteger t, IsStorable t n) => ElementwiseOp (SRegs t n) Int (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsInteger t, IsStorable t n) => ElementwiseOp Int (ScalarExpr t n) (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsInteger t, IsStorable t n) => ElementwiseOp (ScalarExpr t n) Int (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp Float (SRegs t n) (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp (SRegs t n) Float (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp Float (ScalarExpr t n) (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp (ScalarExpr t n) Float (ScalarExpr t n) where
  a + b = (liftScalar a :: (ScalarExpr t n)) + (liftScalar b :: (ScalarExpr t n))
  a - b = (liftScalar a :: (ScalarExpr t n)) - (liftScalar b :: (ScalarExpr t n))
  a * b = (liftScalar a :: (ScalarExpr t n)) * (liftScalar b :: (ScalarExpr t n))
  a & b = (liftScalar a :: (ScalarExpr t n)) & (liftScalar b :: (ScalarExpr t n))
  a << b = (liftScalar a :: (ScalarExpr t n)) << (liftScalar b :: (ScalarExpr t n))

instance (IsInteger t, IsStorable t n) => ElementwiseOp Int (VRegs t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsInteger t, IsStorable t n) => ElementwiseOp (VRegs t n) Int (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsInteger t, IsStorable t n) => ElementwiseOp Int (VectorExpr t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsInteger t, IsStorable t n) => ElementwiseOp (VectorExpr t n) Int (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp Float (VRegs t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp (VRegs t n) Float (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp Float (VectorExpr t n) (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance (IsFloatingPoint t, IsStorable t n) => ElementwiseOp (VectorExpr t n) Float (VectorExpr t n) where
  a + b = (liftVector a :: (VectorExpr t n)) + (liftVector b :: (VectorExpr t n))
  a - b = (liftVector a :: (VectorExpr t n)) - (liftVector b :: (VectorExpr t n))
  a * b = (liftVector a :: (VectorExpr t n)) * (liftVector b :: (VectorExpr t n))
  a & b = (liftVector a :: (VectorExpr t n)) & (liftVector b :: (VectorExpr t n))
  a << b = (liftVector a :: (VectorExpr t n)) << (liftVector b :: (VectorExpr t n))

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (SRegs t n) (SRegs u n) (ScalarExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (SRegs t n) (ScalarExpr u n) (ScalarExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (ScalarExpr t n) (SRegs u n) (ScalarExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (ScalarExpr t n) (ScalarExpr u n) (ScalarExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (SRegs t n) (VRegs u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (SRegs t n) (VectorExpr u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (ScalarExpr t n) (VRegs u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (ScalarExpr t n) (VectorExpr u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (VRegs t n) (SRegs u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (VRegs t n) (ScalarExpr u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (VRegs t n) (VRegs u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (VRegs t n) (VectorExpr u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (VectorExpr t n) (SRegs u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (VectorExpr t n) (ScalarExpr u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (VectorExpr t n) (VRegs u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined

instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( Text "Cannot perform operations on elements of different types: " :<>: ShowType t :<>: Text " and " :<>: ShowType u
          :$$: Text "If this operation is intended, use an explicit type cast: cast @" :<>: ShowType t :<>: Text " myvar"
      ),
    (t == u) ~ False
  ) =>
  ElementwiseOp (VectorExpr t n) (VectorExpr u n) (VectorExpr t n)
  where
  a + b = undefined
  a - b = undefined
  a * b = undefined
  a & b = undefined
  a << b = undefined
