program intrinsics_166
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    integer, parameter :: i1 = ibset(5, 8)
    integer, parameter :: i2 = ibset(-1_8, 5)
    integer, parameter :: i3 = ibset(-4, 2_8)
    integer(8), parameter :: i4 = ibset(-2_8, 5_8)

    integer, parameter :: ar1(3) = ibset([5, 8, 9], [8, 5, 2])
    integer(8), parameter :: ar2(3) = ibset([-1_8, -5_8, -10_8], [5, 8, 9])

    integer :: arr1(3), arr3(3)
    integer(8) :: arr2(3)
    integer :: res(3)
    arr1 = [5, 8, 9]
    arr3 = [8, 5, 2]
    arr2 = [-1_8, -5_8, -10_8]

    print *, i1
    if (i1 /= 261) error stop
    print *, i2
    if (i2 /= -1) error stop
    print *, i3
    if (i3 /= -4) error stop
    print *, i4
    if (i4 /= -2) error stop

    print*, ibset(5, 8)
    if (ibset(5, 8) /= 261) error stop
    print*, ibset(-1, 5)
    if (ibset(-1, 5) /= -1) error stop
  
    print*, ibset(a1, a2)
    if (ibset(a1, a2) /= 261) error stop
    print*, ibset(a3, a1)
    if (ibset(a3, a1) /= -1) error stop
    print*, ibset(a2, a4)
    if (ibset(a2, a4) /= 268435464) error stop
    print*, ibset(a5, a6)
    if (ibset(a5, a6) /= -2) error stop

    print *, ar1
    if (any(ar1 /= [261, 40, 13])) error stop
    print *, ar2
    if (any(ar2 /= [-1, -5, -10])) error stop

    print *, ibset(arr1, arr1)
    if (any(ibset(arr1, arr1) /= [37, 264, 521])) error stop
    print *, ibset(arr2, arr1)
    if (any(ibset(arr2, arr1) /= [-1, -5, -10])) error stop

    res = ibset(arr1, arr3)
    print *, res
    if (any(res /= [261, 40, 13])) error stop

end program 
  
