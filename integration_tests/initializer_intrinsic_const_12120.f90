program initializer_intrinsic_const_12120
    implicit none
    integer, parameter :: a = 7
    integer, parameter :: b = 3
    integer, parameter :: c = mod(a, b)
    print *, c
    if (c /= 1) error stop
end program
