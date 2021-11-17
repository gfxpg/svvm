bb0:
// IR 0: IScalarMoveN ('I32)%s[0:1] 1
s_mov_b32 =%s0, 0x00000001
s_mov_b32 =%s1, 0x00000000
// IR 1: IScalarMoveN ('I32)%s[2] ('I32)s[8]
s_mov_b32 =%s2, s8
// IR 2: IScalarMoveN ('I32)%s[5:9] ('I32)%s[0:4]
s_mov_b64 =%s[5:6], %s[0:1]
s_mov_b64 =%s[7:8], %s[2:3]
s_mov_b32 =%s9, %s4
bb1:

-- Virtual SGPRs (%s[...]):
---- Block [0:4], alignment 2
---- Block [5:9], alignment 2
-- Virtual VGPRs (%v[...]):
