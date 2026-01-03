program intrinsics_162
    implicit none
    integer :: a1 = 1
    integer(8) :: a2 = -5
    integer :: a3 = 0
    integer, parameter :: i1 = not(1)
    integer(8), parameter :: i2 = not(-5)
    integer, parameter :: ar1(3) = not([-1, 43, -8])
    integer(8), parameter :: ar2(3) = not([-1_8, 43_8, -8_8])

    integer(4) :: arr1(3) = [11, -13, 0]
    integer(8) :: arr2(3) = [11_8, -13_8, 0_8]

    print *, i1
    if (i1 /= -2) error stop
    print *, i2
    if (i2 /= 4) error stop
    print *, ar1
    if (any(ar1 /= [0, -44, 7])) error stop
    print *, ar2
    if (any(ar2 /= [0_8, -44_8, 7_8])) error stop

    print*, not(a1)
    if (not(a1) /= -2) error stop
    print*, not(a2)
    if (not(a2) /= 4) error stop
    print*, not(a3)
    if (not(a3) /= -1) error stop

    print*, not(1)
    if (not(1) /= -2) error stop
    print*, not(-5)
    if (not(-5) /= 4) error stop
    print*, not(0)
    if (not(0) /= -1) error stop

    print*, not(arr1)
    if (any(not(arr1) /= [-12, 12, -1])) error stop
    print*, not(arr2)
    if (any(not(arr2) /= [-12_8, 12_8, -1_8])) error stop

    print *, kind(not(1))
    if (kind(not(1)) /= 4) error stop
    print *, kind(not(5_8))
    if (kind(not(5_8)) /= 8) error stop

end program
