program test

implicit none
integer, parameter :: RP = kind(0.d0)
real(kind=RP), parameter :: PI = 3.141592653589793_RP
write(unit=*, fmt=*) PI

if (abs(PI - 3.141592653589793_RP) > 1e-6_RP) error stop

end program test
