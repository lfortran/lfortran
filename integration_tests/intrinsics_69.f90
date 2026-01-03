program intrinsics_69
    implicit none
    integer, parameter :: a1 = radix(3)
    integer, parameter :: a2 = radix(1.12)
    integer, parameter :: a3 = radix([1, 2, 3])
    integer, parameter :: a4 = radix([1.5, 21.12, 3.53])
    integer :: x1 = 1
    real :: x2 = 22.13
    integer :: y1(2) = [1, 2]
    real :: y2(2) = [2., 4.]

    print *, a1
    if (a1 /= 2) error stop
    print *, a2
    if (a2 /= 2) error stop
    print *, a3
    if (a3 /= 2) error stop
    print *, a4
    if (a4 /= 2) error stop

    print *, radix(3)
    if (radix(3) /= 2) error stop
    print *, radix(x1)
    if (radix(x1) /= 2) error stop
    print *, radix(y1)
    if (radix(y1) /= 2) error stop
    print *, radix(x2)
    if (radix(x2) /= 2) error stop
    print *, radix(y2)
    if (radix(y2) /= 2) error stop
    
end program intrinsics_69
