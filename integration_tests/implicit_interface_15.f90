program implicit_interface_15
real a(3)
a = [1.0, 12.512, -3.512]
call stfsm(a)
end program


subroutine stfsm( a )
real :: a( 0: * )
external :: strsm
call strsm( a, 3 )
return
end
