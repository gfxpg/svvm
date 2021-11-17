bb0:
// IR 0: IScalarArithN 'ArithMul ('I32)%s[0:1] ('I32)%s[0:1] 5
s_mul_i32 =%s0, %s0, 0x00000005
s_mul_i32 =%s1, %s1, 0x00000005
// IR 1: IScalarArithN 'ArithMul ('I32)%s[0] ('I32)%s[0] ('I32)%s[1]
s_mul_i32 =%s0, %s0, %s1
bb1:

-- Virtual SGPRs (%s[...]):
---- Block [0:1], alignment 1
-- Virtual VGPRs (%v[...]):
