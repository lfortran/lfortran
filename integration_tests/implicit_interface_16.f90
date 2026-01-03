program implicit_interface_16 

implicit none

real :: v(4)
logical, external :: sisnan

v = [1.0, 2.0, 3.0, 4.0]

print *, sisnan( v( 1 ) )
if( sisnan( v( 1 ) ) ) error stop

end program
