program bits_06
implicit none

integer(1) :: int1 = 1
integer(2) :: int2 = 4
integer(4) :: int4 = 41
integer(8) :: int8 = 411

integer :: size1, size2, size4, size8

size1 = bit_size(int1)
if (size1 /= 8) error stop

size2 = bit_size(int2)
if (size2 /= 16) error stop

size4 = bit_size(int4)
if (size4 /= 32) error stop

size8 = bit_size(int8)
if (size8 /= 64) error stop

print *, bit_size(int1)
print *, bit_size(int2)
print *, bit_size(int4)
print *, bit_size(int8)

end program
