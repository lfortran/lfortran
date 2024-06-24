program intrinsics_160
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    integer, parameter :: i1 = ior(5, 8)
    integer, parameter :: i2 = ior(-1, 5)
    integer, parameter :: i3 = ior(-4_8, 2_8)
    integer(8), parameter :: i4 = ior(-2_8, 5_8)

    integer, parameter :: ar1(3) = ior([5, 8, 9], [8, 5, 2])
    integer(8), parameter :: ar2(3) = ior([-1_8, -5_8, -10_8], [5_8, 8_8, 9_8])

    integer :: arr1(3), arr3(3)
    integer(8) :: arr2(3)
    integer :: res(3)
    arr1 = [5, 8, 9]
    arr3 = [8, 5, 2]
    arr2 = [-1_8, -5_8, -10_8]

    print *, i1
    if (i1 /= 13) error stop
    print *, i2
    if (i2 /= -1) error stop
    print *, i3
    if (i3 /= -2) error stop
    print *, i4
    if (i4 /= -1) error stop

    print*, ior(5, 8)
    if (ior(5, 8) /= 13) error stop
    print*, ior(-1, 5)
    if (ior(-1, 5) /= -1) error stop
    print*, ior(8, -4)
    if (ior(8, -4) /= -4) error stop
    print*, ior(-2, -5)
    if (ior(-2, -5) /= -1) error stop
  
    print*, ior(a1, a2)
    if (ior(a1, a2) /= 13) error stop
    print*, ior(a3, a4)
    if (ior(a3, a4) /= -1) error stop
    print*, ior(a2, a5)
    if (ior(a2, a5) /= -2) error stop
    print*, ior(a5, a6)
    if (ior(a5, a6) /= -1) error stop

    print *, ar1
    if (any(ar1 /= [13, 13, 11])) error stop
    print *, ar2
    if (any(ar2 /= [-1, -5, -1])) error stop

    print *, ior(arr1, arr3)
    if (any(ior(arr1, arr3) /= [13, 13, 11])) error stop
    print *, ior(arr2, arr2)
    if (any(ior(arr2, arr2) /= [-1, -5, -10])) error stop

    res = ior(arr1, arr3)
    print *, res
    if (any(res /= [13, 13, 11])) error stop

end program 