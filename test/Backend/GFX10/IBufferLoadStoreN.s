bb0:
// IR 0: IBufferLoadN ('I32)%v[1:9] ('U32)%v[0] ('U32)%s[0:3] 0
buffer_load_dwordx4 =%v[1:4], %v0, %s[0:3], 0x00000000 offen offset:0
buffer_load_dwordx4 =%v[5:8], %v0, %s[0:3], 0x00000000 offen offset:16
buffer_load_dword =%v9, %v0, %s[0:3], 0x00000000 offen offset:32
// IR 1: IBufferStoreN ('I32)%v[11:13] ('U32)%v[10] ('U32)%s[4:7] 0
buffer_store_dwordx3 %v[11:13], %v10, %s[4:7], 0x00000000 offen offset:0
bb1:

-- Virtual SGPRs (%s[...]):
---- Block [0:3], alignment 4
---- Block [4:7], alignment 4
-- Virtual VGPRs (%v[...]):
---- Block [0:0], alignment 1
---- Block [1:9], alignment 1
---- Block [10:10], alignment 1
---- Block [11:13], alignment 1
