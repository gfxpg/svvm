bb0:
// IR 0: IVectorMoveN ('I32)%v[0] 13
v_mov_b32 =%v0, 0x0000000d
// IR 1: IVectorMoveN ('I32)%v[1] ('I32)%v[0]
v_mov_b32 =%v1, %v0
// IR 2: IVectorMoveN ('I32)%v[1:2] 2170205188308785882
v_mov_b32 =%v1, 0xdadadada
v_mov_b32 =%v2, 0x1e1e1e1e
// IR 3: IVectorMoveN ('I32)%v[3:4] ('I32)s[6:7]
v_mov_b32 =%v3, s6
v_mov_b32 =%v4, s7
bb1:

-- Virtual SGPRs (%s[...]):
-- Virtual VGPRs (%v[...]):
---- Block [0:0], alignment 1
---- Block [1:4], alignment 1
