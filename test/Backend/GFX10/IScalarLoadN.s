bb0:
// IR 0: IScalarLoadN ('U32)%s[0:46] ('U32)s[6:7] 4
s_load_dwordx16 =%s[0:15], s[6:7], 0x00000004
s_load_dwordx16 =%s[16:31], s[6:7], 0x00000044
s_load_dwordx8 =%s[32:39], s[6:7], 0x00000084
s_load_dwordx4 =%s[40:43], s[6:7], 0x000000a4
s_load_dwordx2 =%s[44:45], s[6:7], 0x000000b4
s_load_dword =%s46, s[6:7], 0x000000bc
bb1:

-- Virtual SGPRs (%s[...]):
---- Block [0:46], alignment 4
-- Virtual VGPRs (%v[...]):
