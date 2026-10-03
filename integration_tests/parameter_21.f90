! A real(4) named constant computed from other constants has the value that
! single precision arithmetic gives, as the same expression evaluated at run
! time: 5*0.9 is 4.5 in real(4), so its nearest integer is 5.
program parameter_21
    implicit none
    integer, parameter :: n = 5
    real, parameter :: x = n*0.9, y = 0.1*3.0, z = 2.0**(-3)
    integer, parameter :: k = nint(n*0.9)
    integer :: m
    real :: a, b
    m = n
    a = 0.1
    b = 3.0
    print *, x, y, z, k
    if (k /= 5) error stop
    if (nint(x) /= nint(m*0.9)) error stop
    if (x /= m*0.9) error stop
    if (y /= a*b) error stop
    if (z /= 0.125) error stop
end program
