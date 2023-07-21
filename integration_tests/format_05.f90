program format_05

real :: a,b,c,d
character(100) :: s
a = 123.456
b = 123.45678
c = 12.34
d = 123.45

print '(-1pe10.2,-2pe15.6,1pe010.2,2x,1pe9.2,f10.3)', -a, b, -c, d, d
write (s, '(-1pe10.2,-2pe15.6,1pe010.2,2x,1pe9.2)') -a, b, -c, d
if (trim(s) /= " -0.01E+04   0.001235E+05 -1.23E+01   1.23E+02") error stop
end program
