program bits_06
implicit none

integer(1) :: int1 = 1
integer(2) :: int2 = 4
integer(4) :: int4 = 41
integer(8) :: int8 = 411

print *, bit_size(int1)
print *, bit_size(int2)
print *, bit_size(int4)
print *, bit_size(int8)

end program
