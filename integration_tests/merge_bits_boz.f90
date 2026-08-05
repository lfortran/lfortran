program merge_bits_boz
use, intrinsic :: iso_fortran_env, only: int16
implicit none
integer(int16) :: r1, r2, r3

r1 = merge_bits(32767_int16, o'12345', 32767_int16)
r2 = merge_bits(o'12345', 32767_int16, b'0000000000010101')
r3 = merge_bits(32767_int16, o'12345', z'1234')

if (r1 /= 32767_int16) error stop
if (r2 /= 32751_int16) error stop
if (r3 /= 5877_int16) error stop

print *, r1, r2, r3
end program merge_bits_boz
