program intrinsics_168
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    integer, parameter :: i1 = ibclr(5, 8)
    integer, parameter :: i2 = ibclr(-1_8, 5)
    integer, parameter :: i3 = ibclr(-4, 2_8)
    integer(8), parameter :: i4 = ibclr(-2_8, 5_8)

    integer, parameter :: ar1(3) = ibclr([5, 8, 9], [8, 5, 2])
    integer(8), parameter :: ar2(3) = ibclr([-1_8, -5_8, -10_8], [5, 8, 9])

    integer :: arr1(3), arr3(3)
    integer(8) :: arr2(3)
    integer :: res(3)
    arr1 = [5, 8, 9]
    arr3 = [8, 5, 2]
    arr2 = [-1_8, -5_8, -10_8]

    print *, i1
    if (i1 /= 5) error stop
    print *, i2
    if (i2 /= -33) error stop
    print *, i3
    if (i3 /= -8) error stop
    print *, i4
    if (i4 /= -34) error stop

    print*, ibclr(5, 8)
    if (ibclr(5, 8) /= 5) error stop
    print*, ibclr(-1, 5)
    if (ibclr(-1, 5) /= -33) error stop

    print*, ibclr(a1, a2)
    if (ibclr(a1, a2) /= 5) error stop

    ! Does not work yet - #4308

    ! print*, ibclr(a3, a1)
    ! if (ibclr(a3, a1) /= -33) error stop
    ! print*, ibclr(a2, a4)
    ! if (ibclr(a2, a4) /= 8) error stop
    print*, ibclr(a5, a6)
    if (ibclr(a5, a6) /= -134217730) error stop

    print *, ar1
    if (any(ar1 /= [5, 8, 9])) error stop
    print *, ar2
    if (any(ar2 /= [-33, -261, -522])) error stop

    print *, ibclr(arr1, arr1)
    if (any(ibclr(arr1, arr1) /= [5, 8, 9])) error stop
    ! print *, ibclr(arr2, arr1
    ! if (any(ibclr(arr2, arr1) /= [-33, -134217730, -134217731])) error stop

    res = ibclr(arr1, arr3)
    print *, res
    if (any(res /= [5, 8, 9])) error stop

end program 