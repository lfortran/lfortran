program format_118
! G editing chooses between F and E editing, and the number of digits after
! the decimal point, from the value rounded to d significant digits. Rounding
! can carry into the next power of ten.
implicit none
character(20) :: s
real :: r

! F editing: the carry adds an integer digit and removes a fraction digit.
write(s, '(g13.5)') 0.9999999999d0
if (s /= '   1.0000    ') error stop 1
write(s, '(g13.5)') 99.9999999d0
if (s /= '   100.00    ') error stop 2
write(s, '(g13.5)') 9.999996d0
if (s /= '   10.000    ') error stop 3
write(s, '(g13.5)') -0.9999999999d0
if (s /= '  -1.0000    ') error stop 4
r = 0.99999999
write(s, '(g13.5)') r
if (s /= '   1.0000    ') error stop 5

! Below 0.1, a value that rounds up to 0.1 is written with F editing.
write(s, '(g13.5)') 0.09999996d0
if (s /= '  0.10000    ') error stop 6

! Values that do not carry are unchanged.
write(s, '(g13.5)') 0.99999d0
if (s /= '  0.99999    ') error stop 7
write(s, '(g13.5)') 99.999d0
if (s /= '   99.999    ') error stop 8
write(s, '(g13.5)') 1.0d0
if (s /= '   1.0000    ') error stop 9

! A value that rounds up to 10**d is written with E editing, and the carry
! moves into the exponent.
write(s, '(g12.4)') 999999.5d0
if (s /= '  0.1000E+07') error stop 10
write(s, '(g12.4)') 9999.7d0
if (s /= '  0.1000E+05') error stop 11
write(s, '(g12.4)') 9.99999999d-11
if (s /= '  0.1000E-09') error stop 12
write(s, '(g12.4)') 9999.4d0
if (s /= '   9999.    ') error stop 13

! With a scale factor, the carry moves into the exponent as well.
write(s, '(1p,g12.4)') 9.99999999d-11
if (s /= '  1.0000E-10') error stop 14
write(s, '(-1p,g12.4)') 999999.7d0
if (s /= '  0.0100E+08') error stop 15
write(s, '(2p,g13.5)') 999999.99d0
if (s /= '  10.0000E+05') error stop 16
end program
