{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (forM_)
import Svvm.Driver
import Svvm.Dsl
import Prelude hiding (Num (..), fromInteger, fromRational, not, (!!), (&&), (<), (||))

main :: IO ()
main = do
  let translated = translateProgram sampleShader
  let translationGraph = showTranslationProcessGraph translated
  let assembly = showProgramAssembly translated
  writeFile "svvm-translation-graph.dot" translationGraph
  putStrLn assembly

sampleShader :: Dsl ()
sampleShader = do
  -- See https://github.com/gfxpg/reki/tree/master/docs#analyzing-the-program-itself
  -- for a more detailed description of physical registers v0, s8, s[6:7]
  v_thread_id :: VReg U32 <- allocPhysical 0
  s_group_id :: SReg U32 <- allocPhysical 8
  s_kernarg_addr :: SRegs U32 2 <- allocPhysical 6 -- 6:7
  s_kernargs :: SRegs U32 7 <- alloc

  let s_in_addr = slice @2 @0 s_kernargs
  let s_out_addr = slice @2 @2 s_kernargs
  let s_size = s_kernargs !! 4
  let s_ngroups = s_kernargs !! 5
  let s_x = s_kernargs !! 6

  s_load_dwordN s_kernargs s_kernarg_addr 0
  s_waitcnt_0

  in_srd :: SRegs U32 4 <- alloc
  slice @2 @0 in_srd .= s_in_addr
  in_srd !! 1 .= (in_srd !! 1) & 0xffff
  in_srd !! 2 .= s_size
  in_srd !! 3 .= 0x804fac

  out_srd :: SRegs U32 4 <- alloc
  slice @2 @0 out_srd .= s_out_addr
  out_srd !! 1 .= (out_srd !! 1) & 0xffff
  out_srd !! 2 .= s_size
  out_srd !! 3 .= 0x804fac

  let group_size = 256

  v_in_offset :: VReg U32 <- alloc
  v_in_offset .= (s_group_id * group_size + v_thread_id) << 2

  v_out_offset :: VReg U32 <- alloc
  v_out_offset .= v_in_offset

  v_step :: VReg U32 <- alloc
  v_step .= (s_ngroups * group_size) << 2

  v_elts :: VRegs F32 4 <- alloc
  v_step .= v_step * dwordCount v_elts

  while_ (v_in_offset < s_size && v_out_offset < s_size) $ do
    buffer_load_dwordN v_elts v_in_offset in_srd 0
    v_in_offset .= v_in_offset + v_step

    s_waitcnt_0

    forM_ [0 .. dwordCount v_elts - 1] $ \i ->
      v_elts !! i .= v_elts !! i + cast @F32 s_x

    buffer_store_dwordN v_elts v_in_offset in_srd 0
    v_out_offset .= v_out_offset + v_step

  endpgm
