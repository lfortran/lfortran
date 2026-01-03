program intrinsics_161
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    integer, parameter :: i1 = ieor(5, 8)
    integer, parameter :: i2 = ieor(-1, 5)
    integer, parameter :: i3 = ieor(-4_8, 2_8)
    integer(8), parameter :: i4 = ieor(-2_8, 5_8)

    integer, parameter :: ar1(3) = ieor([5, 8, 9], [8, 5, 2])
    integer(8), parameter :: ar2(3) = ieor([-1_8, -5_8, -10_8], [5_8, 8_8, 9_8])

    integer :: arr1(3), arr3(3)
    integer(8) :: arr2(3)
    integer :: res(3)
    arr1 = [5, 8, 9]
    arr3 = [8, 5, 2]
    arr2 = [-1_8, -5_8, -10_8]

    print *, i1
    if (i1 /= 13) error stop
    print *, i2
    if (i2 /= -6) error stop
    print *, i3
    if (i3 /= -2) error stop
    print *, i4
    if (i4 /= -5) error stop

    print*, ieor(5, 8)
    if (ieor(5, 8) /= 13) error stop
    print*, ieor(-1, 5)
    if (ieor(-1, 5) /= -6) error stop
    print*, ieor(8, -4)
    if (ieor(8, -4) /= -12) error stop
    print*, ieor(-2, -5)
    if (ieor(-2, -5) /= 5) error stop
  
    print*, ieor(a1, a2)
    if (ieor(a1, a2) /= 13) error stop
    print*, ieor(a3, a4)
    if (ieor(a3, a4) /= 3) error stop
    print*, ieor(a2, a5)
    if (ieor(a2, a5) /= -10) error stop
    print*, ieor(a5, a6)
    if (ieor(a5, a6) /= 5) error stop

    print *, ar1
    if (any(ar1 /= [13, 13, 11])) error stop
    print *, ar2
    if (any(ar2 /= [-6, -13, -1])) error stop

    print *, ieor(arr1, arr3)
    if (any(ieor(arr1, arr3) /= [13, 13, 11])) error stop
    print *, ieor(arr2, arr2)
    if (any(ieor(arr2, arr2) /= [0, 0, 0])) error stop

    res = ieor(arr1, arr3)
    print *, res
    if (any(res /= [13, 13, 11])) error stop

end program 