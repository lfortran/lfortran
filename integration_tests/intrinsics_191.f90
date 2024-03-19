program intrinsics_191
    implicit none
    complex :: a
    integer :: res
    a = cmplx(1)
    res = kind(a)
    print *, res
    if (res /= 4) error stop
end program
