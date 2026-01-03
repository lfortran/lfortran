program implicit_interface_17
implicit none

real :: y( 5,5 )

external :: isamax
integer :: isamax

y = 1.0
y( 4, 1 ) = 19.68

print *, y( isamax(2, y( 1, 1 ), 1 ), 1 )
if ( abs(y(isamax(2, y(1,1), 1), 1 ) - 19.68) > 1e-8 ) error stop

end 
