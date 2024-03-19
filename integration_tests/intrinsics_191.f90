program intrinsics_191
    implicit none
    complex :: a
    complex(kind=8) :: b
    integer :: res
    a = cmplx(1)
    res = kind(a)
    print *, res
    if (res /= 4) error stop
    b = cmplx(1)
    res = kind(b)
    print *, res
    if (res /= 8) error stop
end program
