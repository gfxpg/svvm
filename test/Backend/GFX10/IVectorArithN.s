bb0:
// IR 0: IVectorArithN 'ArithAdd ('U32)%v[0:1] ('U32)%v[0:1] ('U32)%s[0:1]
v_add_nc_u32 =%v0, %v0, %s0
v_add_nc_u32 =%v1, %v1, %s1
// IR 1: IVectorArithN 'ArithAdd ('U32)%v[0] ('U32)%v[0] 10
v_add_nc_u32 =%v0, %v0, 0x0000000a
// IR 2: IVectorArithN 'ArithMul ('U32)%v[0] ('U32)%v[0] ('U32)%s[0]
v_mul_lo_u32 =%v0, %v0, %s0
// IR 3: IVectorArithN 'ArithAdd ('I32)%v[0:1] ('I32)%v[0:1] ('I32)%s[0:1]
v_add_nc_i32 =%v0, %v0, %s0
v_add_nc_i32 =%v1, %v1, %s1
// IR 4: IVectorArithN 'ArithAdd ('F32)%v[0] ('F32)%v[0] ('F32)%s[0]
v_add_f32 =%v0, %v0, %s0
// IR 5: IVectorArithN 'ArithAdd ('F32)%v[0] ('F32)%v[0] 3.13
v_add_f32 =%v0, %v0, 0x404851ec
// IR 6: IVectorArithN 'ArithSub ('F32)%v[0] ('F32)%v[0] ('F32)%v[0]
v_sub_f32 =%v0, %v0, %v0
// IR 7: IVectorArithN 'ArithMul ('F32)%v[0] ('F32)%v[0] ('F32)%v[0]
v_mul_f32 =%v0, %v0, %v0
bb1:

-- Virtual SGPRs (%s[...]):
---- Block [0:1], alignment 1
-- Virtual VGPRs (%v[...]):
---- Block [0:1], alignment 1
