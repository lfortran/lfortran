program parameter_14
real :: x
parameter ( x = 12.9E+0 )
print *, x
if ( abs( x - 12.9 ) > 1e-8 ) error stop
end

