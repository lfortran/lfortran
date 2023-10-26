program bits_06
implicit none

integer(2) :: int2 = 4
integer(4) :: int4 = 41
integer(8) :: int8 = 411
integer(16) :: int16 = 411111

write(*, *) "Bit length of int2: ", bit_size(int2)
write(*, *) "Bit length of int4: ", bit_size(int4)
write(*, *) "Bit length of int8: ", bit_size(int8)
write(*, *) "Bit length of int8: ", bit_size(int16)

end program
