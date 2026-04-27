program format_99
implicit none
double precision :: x
character(10) :: s
character(15) :: s2

! Test G format E-mode: leading zero should be omitted to fit field width
x = 85.7343D6
write(s, '(G10.5E2)') x
if (s /= ".85734E+08") error stop
print *, s

! Test with wider field (leading zero fits)
x = 85.7343D6
write(s2, '(G15.5E2)') x
if (s2 /= "    0.85734E+08") error stop
print *, s2

! Test G format with default exponent where leading zero fits exactly
x = 1.234D10
write(s, '(G10.4)') x
if (s /= "0.1234E+11") error stop
print *, s

print *, "OK"
end program
