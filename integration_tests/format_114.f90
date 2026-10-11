program format_114
! The decimal exponent of E, ES and D editing is that of the printed digits,
! also for values just below a power of ten
implicit none
real(8) :: x, y
character(len=40) :: s

x = 999999999999999.0_8
y = 1.0e23_8

write(s, "(es12.4)") x
if (s /= "  1.0000E+15") error stop
write(s, "(e12.4)") x
if (s /= "  0.1000E+16") error stop
write(s, "(d12.4)") x
if (s /= "  0.1000D+16") error stop
write(s, "(es25.17)") x
if (s /= "  9.99999999999999000E+14") error stop
write(s, "(e30.20)") x
if (s /= "    0.99999999999999900000E+15") error stop
write(s, "(1p,e12.4)") x
if (s /= "  1.0000E+15") error stop

write(s, "(es12.4)") y
if (s /= "  1.0000E+23") error stop
write(s, "(e12.4)") y
if (s /= "  0.1000E+24") error stop
write(s, "(d12.4)") y
if (s /= "  0.1000D+24") error stop
write(s, "(es25.17)") y
if (s /= "  9.99999999999999916E+22") error stop
write(s, "(e30.20)") y
if (s /= "    0.99999999999999991611E+23") error stop
end program
