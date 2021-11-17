bb0:
// IR 0: IVectorBitwiseN 'BitwShl ('U32)%v[0:1] ('U32)%v[0:1] ('U32)%s[0:1]
v_lshrrev_b32 =%v0, %s0, %v0
v_lshrrev_b32 =%v1, %s1, %v1
// IR 1: IVectorBitwiseN 'BitwShl ('U32)%v[0] ('U32)%v[0] 10
v_lshrrev_b32 =%v0, 0x0000000a, %v0
bb1:

-- Virtual SGPRs (%s[...]):
---- Block [0:1], alignment 1
-- Virtual VGPRs (%v[...]):
---- Block [0:1], alignment 1
