{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DslSpec where

import Control.Monad (forM_, replicateM)
import Control.Monad.State (execState)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word32)
import Svvm.Dsl
import Svvm.Instructions
import Test.Hspec
import Svvm.Utils.TypeUnion
import Prelude hiding (Num (..), fromInteger, fromRational, not, (!!), (&&), (<), (||))

runProgram :: Dsl () -> ([(Int, Instruction)], [(Int, LabelTarget)])
runProgram pgm = (indexed (sPgm state), indexedJust (sLabelTargets state))
  where
    indexedJust = zip [0 ..] . (fromJust <$>) . toList
    indexed = zip [0 ..] . toList
    state = execState pgm initDslState

spec :: Spec
spec = describe "dsl" $ do
  it "can cast between types" $ do
    let (instructions, labels) = runProgram $ do
          kernarg_addr :: SRegs U32 2 <- allocPhysical 6 -- 6:7
          s_kernargs :: SRegs U32 10 <- alloc

          s_load_dwordN s_kernargs kernarg_addr 0

          s_x :: SReg I32 <- alloc
          let s_y = cast @I32 (s_kernargs !! 7)

          s_x .= s_x + s_y

          v_ux :: VReg U32 <- alloc
          let v_ix = cast @I32 v_ux

          v_ix .= s_x

    show <$> instructions
      `shouldBe` [ "(0,IScalarLoadN ('U32)%s[0:9] ('U32)s[6:7] 0)",
                   "(1,IScalarArithN 'ArithAdd ('I32)%s[10] ('I32)%s[10] ('I32)%s[7])",
                   "(2,IVectorMoveN ('I32)%v[0] ('I32)%s[10])"
                 ]

  it "supports scalar memory operations" $ do
    let (instructions, labels) = runProgram $ do
          kernarg :: SRegs U32 2 <- allocPhysical 6 -- 6:7
          sdw2 :: SRegs I32 2 <- alloc
          sdw4 :: SRegs I32 4 <- alloc
          sdw8 :: SRegs U32 8 <- alloc
          sdw1 :: SReg I32 <- alloc
          s_load_dwordN sdw2 kernarg 0
          s_load_dwordN sdw4 kernarg (2 * 4)
          s_load_dwordN sdw8 kernarg (6 * 4)
          s_load_dwordN sdw1 kernarg (14 * 4)
    show <$> instructions
      `shouldBe` [ "(0,IScalarLoadN ('I32)%s[0:1] ('U32)s[6:7] 0)",
                   "(1,IScalarLoadN ('I32)%s[2:5] ('U32)s[6:7] 8)",
                   "(2,IScalarLoadN ('U32)%s[6:13] ('U32)s[6:7] 24)",
                   "(3,IScalarLoadN ('I32)%s[14] ('U32)s[6:7] 56)"
                 ]

  it "supports vector operations and vccz-based scalar control flow" $ do
    let (instructions, labels) = runProgram $ do
          v0 :: VReg I32 <- alloc
          v1 :: VReg I32 <- alloc
          s0 :: SReg I32 <- alloc

          -- if then else is evaluated statically
          let const = if not (True && False) then 5 + 3 else 0

          v0 .= const * s0 -- vector = scalar
          v0 .= v0 * s0 -- vector = vector (op) scalar

          -- if_ (v0 < 0) $ do
          -- v1 .= v0 << 4
          v0 .= s0
    show <$> instructions
      `shouldBe` [ "(0,IScalarArithN 'ArithMul ('I32)%s[1] 8 ('I32)%s[0])",
                   "(1,IVectorMoveN ('I32)%v[0] ('I32)%s[1])",
                   "(2,IVectorArithN 'ArithMul ('I32)%v[0] ('I32)%v[0] ('I32)%s[0])",
                   "(3,IVectorMoveN ('I32)%v[0] ('I32)%s[0])"
                 ]
    labels
      `shouldBe` []

  it "supports scalar operations and control flow" $ do
    let (instructions, labels) = runProgram $ do
          s0 :: SReg I32 <- alloc
          s1 :: SReg I32 <- alloc
          v0 :: VReg I32 <- alloc
          let a = 5 + 3
          s0 .= a + s0 + (1 + 1)
          if_ (s0 < 5 + a && s1 < 10) $ do
            s1 .= a + 10 - s0
            s1 .= 6 & 3 - s1
            s1 .= s1 - 1 << 2
            s1 .= s0 + 7 - s0 * (9 * 3)
          v0 .= s1 -- ok
          --s1 .= v0 -- NOT ok
    show <$> instructions
      `shouldBe` [ "(0,IScalarArithN 'ArithAdd ('I32)%s[0] 8 ('I32)%s[0])",
                   "(1,IScalarArithN 'ArithAdd ('I32)%s[0] ('I32)%s[0] 2)",
                   "(2,IScalarCmp 'CmpLt ('I32)%s[0] 13)",
                   "(3,ICondBranch BrNotScc (Label 1))",
                   "(4,IScalarCmp 'CmpLt ('I32)%s[1] 10)",
                   "(5,ICondBranch BrNotScc (Label 0))",
                   "(6,IScalarArithN 'ArithSub ('I32)%s[1] 18 ('I32)%s[0])",
                   "(7,IScalarArithN 'ArithSub ('I32)%s[1] 2 ('I32)%s[1])", -- s1 = 6&3-s1
                   "(8,IScalarArithN 'ArithSub ('I32)%s[1] ('I32)%s[1] 1)", -- s1 = s1-1
                   "(9,IScalarBitwiseN 'BitwShl ('I32)%s[1] ('I32)%s[1] 2)", -- s1 = s1 << 2
                   "(10,IScalarArithN 'ArithAdd ('I32)%s[1] ('I32)%s[0] 7)", -- s1 = s0+7,
                   "(11,IScalarArithN 'ArithMul ('I32)%s[2] ('I32)%s[0] 27)", -- s2 = s0*9*3
                   "(12,IScalarArithN 'ArithSub ('I32)%s[1] ('I32)%s[1] ('I32)%s[2])", -- s1 = s1 - s2
                   "(13,IVectorMoveN ('I32)%v[0] ('I32)%s[1])"
                 ]
    labels
      `shouldBe` [(0, LabelTarget 13), (1, LabelTarget 5)]
