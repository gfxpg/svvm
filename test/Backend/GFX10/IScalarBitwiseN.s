bb0:
// IR 0: IScalarBitwiseN 'BitwAnd ('I32)%s[0:1] ('I32)%s[0:1] 65535
s_and_b32 =%s0, %s0, 0x0000ffff
s_and_b32 =%s1, %s1, 0x0000ffff
// IR 1: IScalarBitwiseN 'BitwAnd ('U64)%s[2:5] 2170205188308785882 ('U64)%s[2:5]
s_and_b32 =%s2, 0xdadadada, %s2
s_and_b32 =%s3, 0x1e1e1e1e, %s3
s_and_b32 =%s4, 0xdadadada, %s4
s_and_b32 =%s5, 0x1e1e1e1e, %s5
// IR 2: IScalarBitwiseN 'BitwAnd ('U64)%s[6:7] ('U64)%s[6:7] ('U64)%s[2:3]
s_and_b64 =%s[6:7], %s[6:7], %s[2:3]
// IR 3: IScalarBitwiseN 'BitwShl ('U64)%s[2:5] ('U64)%s[2:5] 2
s_lshl_b64 =%s[2:3], %s[2:3], 0x00000002
s_lshl_b64 =%s[4:5], %s[4:5], 0x00000002
// IR 4: IScalarBitwiseN 'BitwShl ('I32)%s[0] ('I32)%s[0] ('I32)%s[0]
s_lshl_b32 =%s0, %s0, %s0
bb1:

-- Virtual SGPRs (%s[...]):
---- Block [0:1], alignment 1
---- Block [2:5], alignment 2
---- Block [6:7], alignment 2
-- Virtual VGPRs (%v[...]):
