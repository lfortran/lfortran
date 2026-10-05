program format_113
! Formatted output of real(10) values with the real edit descriptors
implicit none
real(10) :: x, y, z, big, a(3), h, n, tiny, small, huge4
character(len=90) :: s

x = 1.0_10/3.0_10
y = 2.0_10/3.0_10*1.0e10_10
y = -y
! A negative real(10) literal crashes the compiler (#13896), so negative
! values are made by negating a variable
h = 2.5_10
n = 0.5_10
n = -n
z = 0.0_10
big = 1.0e3000_10
a = [x, 1.5_10, 2.0_10/3.0_10]

write(s, "(f8.3,es12.4)") z, z
print "(a)", trim(s)
if (s /= "   0.000  0.0000E+00") error stop

! All the digits of the extended precision value are printed
write(s, "(es30.20)") x
print "(a)", trim(s)
if (s /= "    3.33333333333333333342E-01") error stop
write(s, "(e30.20)") x
if (s /= "    0.33333333333333333334E+00") error stop
write(s, "(d30.20)") x
if (s /= "    0.33333333333333333334D+00") error stop
write(s, "(f30.22)") x
if (s /= "      0.3333333333333333333424") error stop
write(s, "(g30.20)") x
if (s /= "    0.33333333333333333334    ") error stop
write(s, "(g0)") x
if (s /= "0.333333333333333333342") error stop
write(s, "(g30.20)") y
if (s /= "    -6666666666.6666666670    ") error stop

write(s, "(f8.3,es12.4,e12.4,en12.3,g12.4,d12.4)") x, x, x, x, x, x
print "(a)", trim(s)
if (s /= "   0.333  3.3333E-01  0.3333E+00 333.333E-03  0.3333      0.3333D+00") &
    error stop
write(s, "(es12.4,e12.4,en12.3,g12.4,d12.4)") y, y, y, y, y
print "(a)", trim(s)
if (s /= " -6.6667E+09 -0.6667E+10  -6.667E+09 -0.6667E+10 -0.6667D+10") error stop
write(s, "(sp,f8.3,es12.4)") x, x
if (s /= "  +0.333 +3.3333E-01") error stop
write(s, "(1p,e14.5)") x
if (s /= "   3.33333E-01") error stop
write(s, "(f0.5)") x
if (s /= ".33333") error stop
write(s, "(3f8.3)") a
if (s /= "   0.333   1.500   0.667") error stop

! Exponents beyond the range of real(8)
write(s, "(es30.18e4)") big
print "(a)", trim(s)
if (s /= "    1.000000000000000000E+3000") error stop
write(s, "(es30.18)") big
if (s /= "******************************") error stop

! Directed rounding
write(s, "(rd,f10.3)") h
if (s /= "     2.500") error stop
write(s, "(rz,f10.3)") h
if (s /= "     2.500") error stop
write(s, "(ru,f10.3)") h
if (s /= "     2.500") error stop
write(s, "(ru,f10.3)") n
if (s /= "    -0.500") error stop
write(s, "(rd,f10.3)") n
if (s /= "    -0.500") error stop
write(s, "(rd,f8.4)") 9.5_10
if (s /= "  9.5000") error stop
write(s, "(rd,f24.20)") 0.125_10
if (s /= "  0.12500000000000000000") error stop
write(s, "(ru,f10.3,rd,f10.3)") x, x
if (s /= "     0.334     0.333") error stop
write(s, "(rd,f30.22)") h
if (s /= "      2.5000000000000000000000") error stop
! 0.95_10 is 0.949999999999999999989157...
write(s, "(rd,f10.3,ru,f10.3)") 0.95_10, 0.95_10
if (s /= "     0.949     0.950") error stop

! EN with more digits than real(8) has
write(s, "(en28.18)") -n
print "(a)", trim(s)
if (s /= "  500.000000000000000000E-03") error stop
write(s, "(en28.18)") n
if (s /= " -500.000000000000000000E-03") error stop
write(s, "(en28.18)") 0.0009765625_10
if (s /= "  976.562500000000000000E-06") error stop

! F with more decimals than fit in a fixed-size buffer
write(s, "(f85.80)") 0.125_10
if (s /= "   0.125" // repeat("0", 77)) error stop

! Negative exponents with four digits
tiny = 1.0e-3000_10
write(s, "(es30.18e4)") tiny
if (s /= "    1.000000000000000000E-3000") error stop
write(s, "(e30.18e4)") tiny
if (s /= "    0.100000000000000000E-2999") error stop
write(s, "(es30.18)") tiny
if (s /= "******************************") error stop

! A rounding carry that changes the number of exponent digits
small = 1.0e-1000_10
small = small * (1.0_10 - 1.0e-15_10)
huge4 = 1.0e1000_10
huge4 = huge4 * (1.0_10 - 1.0e-15_10)
write(s, "(e12.4)") small
print "(a)", trim(s)
if (s /= "  0.1000-999") error stop
write(s, "(d12.4)") small
if (s /= "  0.1000-999") error stop
write(s, "(es12.4)") small
if (s /= "************") error stop
write(s, "(es12.4)") huge4
if (s /= "************") error stop
write(s, "(sp,es12.4)") huge4
if (s /= "************") error stop
end program
