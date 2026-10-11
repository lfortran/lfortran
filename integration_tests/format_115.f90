program format_115
! F editing with more decimals than fit in a fixed-size buffer
implicit none
real(8) :: x, y
character(len=100) :: s

x = 0.125_8
y = 0.1_8

write(s, "(f85.80)") x
if (s /= "   0.125" // repeat("0", 77)) error stop
write(s, "(f70.62)") x
if (s /= "      0.125" // repeat("0", 59)) error stop
write(s, "(f85.80)") y
if (s /= "   0.1000000000000000055511151231257827021181583404541015625" &
        // repeat("0", 25)) error stop
end program
