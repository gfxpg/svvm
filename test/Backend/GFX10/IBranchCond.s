bb0:
// IR 0: IVectorCmp 'CmpLt ('U32)%v[0] ('U32)%s[0]
v_cmp_lt_u32 vcc, %v0, %s0
// IR 1: ICondBranch BrNotVcc (Label 1)
s_cbranch_vccz bb2
bb1:
// IR 2: IVectorArithN 'ArithAdd ('U32)%v[0] ('U32)%v[0] 4
v_add_nc_u32 =%v0, %v0, 0x00000004
// IR 3: IBranch (Label 0)
s_branch bb0
bb2:
// IR 4: IEndProgram
s_endpgm
bb3:

-- Virtual SGPRs (%s[...]):
---- Block [0:0], alignment 1
-- Virtual VGPRs (%v[...]):
---- Block [0:0], alignment 1
