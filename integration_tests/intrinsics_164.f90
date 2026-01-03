program intrinsics_164
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    integer, parameter :: i1 = iand(5, 8)
    integer, parameter :: i2 = iand(-1, 5)
    integer, parameter :: i3 = iand(-4_8, 2_8)
    integer(8), parameter :: i4 = iand(-2_8, 5_8)

    integer, parameter :: ar1(3) = iand([5, 8, 9], [8, 5, 2])
    integer(8), parameter :: ar2(3) = iand([-1_8, -5_8, -10_8], [5_8, 8_8, 9_8])

    integer :: arr1(3), arr3(3)
    integer(8) :: arr2(3)
    integer :: res(3)
    arr1 = [5, 8, 9]
    arr3 = [8, 5, 2]
    arr2 = [-1_8, -5_8, -10_8]

    print *, i1
    if (i1 /= 0) error stop
    print *, i2
    if (i2 /= 5) error stop
    print *, i3
    if (i3 /= 0) error stop
    print *, i4
    if (i4 /= 4) error stop

    print*, iand(5, 8)
    if (iand(5, 8) /= 0) error stop
    print*, iand(-1, 5)
    if (iand(-1, 5) /= 5) error stop
    print*, iand(8, -4)
    if (iand(8, -4) /= 8) error stop
    print*, iand(-2, -5)
    if (iand(-2, -5) /= -6) error stop
  
    print*, iand(a1, a2)
    if (iand(a1, a2) /= 0) error stop
    print*, iand(a3, a4)
    if (iand(a3, a4) /= -4) error stop
    print*, iand(a2, a5)
    if (iand(a2, a5) /= 8) error stop
    print*, iand(a5, a6)
    if (iand(a5, a6) /= -6) error stop

    print *, ar1
    if (any(ar1 /= [0, 0, 0])) error stop
    print *, ar2
    if (any(ar2 /= [5, 8, 0])) error stop

    print *, iand(arr1, arr3)
    if (any(iand(arr1, arr3) /= [0, 0, 0])) error stop
    print *, iand(arr2, arr2)
    if (any(iand(arr2, arr2) /= [-1, -5, -10])) error stop

    res = iand(arr1, arr3)
    print *, res
    if (any(res /= [0, 0, 0])) error stop

end program 