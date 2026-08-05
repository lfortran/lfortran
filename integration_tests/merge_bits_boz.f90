program merge_bits_boz
use, intrinsic :: iso_fortran_env, only: int16
implicit none
integer(int16) :: r1, r2, r3
integer(int16) :: i1, i2, i3, i4, i5

r1 = merge_bits(32767_int16, o'12345', 32767_int16)
r2 = merge_bits(o'12345', 32767_int16, b'0000000000010101')
r3 = merge_bits(32767_int16, o'12345', z'1234')

if (r1 /= 32767_int16) error stop
if (r2 /= 32751_int16) error stop
if (r3 /= 5877_int16) error stop

i1 = iand(o'12345', 32767_int16)
if (i1 /= iand(int(o'12345', int16), 32767_int16)) error stop

i2 = ior(o'12345', 32767_int16)
if (i2 /= ior(int(o'12345', int16), 32767_int16)) error stop

i3 = ieor(o'12345', 32767_int16)
if (i3 /= ieor(int(o'12345', int16), 32767_int16)) error stop

i4 = dshiftl(o'12345', 32767_int16, 5)
if (i4 /= dshiftl(int(o'12345', int16), 32767_int16, 5)) error stop

i5 = dshiftr(o'12345', 32767_int16, 5)
if (i5 /= dshiftr(int(o'12345', int16), 32767_int16, 5)) error stop

print *, r1, r2, r3
print *, i1, i2, i3, i4, i5
end program merge_bits_boz
