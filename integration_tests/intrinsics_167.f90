program intrinsics_167
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    logical, parameter :: i1 = btest(5, 8)
    logical, parameter :: i2 = btest(-1_8, 5)
    logical, parameter :: i3 = btest(-4, 2_8)
    logical, parameter :: i4 = btest(-2_8, 5_8)

    logical, parameter :: ar1(3) = btest([5, 8, 9], [8, 5, 2])
    logical(8), parameter :: ar2(3) = btest([-1_8, -5_8, -10_8], [5, 8, 9])

    integer :: arr1(3), arr3(3)
    integer(8) :: arr2(3)
    logical :: res(3)
    arr1 = [5, 8, 9]
    arr3 = [8, 5, 2]
    arr2 = [-1_8, -5_8, -10_8]

    print *, i1
    if (i1 .neqv. .false.) error stop
    print *, i2
    if (i2 .neqv. .true.) error stop
    print *, i3
    if (i3 .neqv. .true.) error stop
    print *, i4
    if (i4 .neqv. .true.) error stop

    print*, btest(5, 8)
    if (btest(5, 8) .neqv. .false.) error stop
    print*, btest(-1, 5)
    if (btest(-1, 5) .neqv. .true.) error stop
  
    print*, btest(a1, a2)
    if (btest(a1, a2) .neqv. .false.) error stop
    print*, btest(a3, a1)
    if (btest(a3, a1) .neqv. .true.) error stop
    print*, btest(a2, a4)
    if (btest(a2, a4) .neqv. .false.) error stop
    print*, btest(a5, a6)
    if (btest(a5, a6) .neqv. .true.) error stop

    print *, ar1
    if (any(ar1 .neqv. [.false., .false., .false.])) error stop
    print *, ar2
    if (any(ar2 .neqv. [.true., .true., .true.])) error stop

    print *, btest(arr1, arr1)
    if (any(btest(arr1, arr1) .neqv. [.false., .false., .false.])) error stop
    print *, btest(arr2, arr1)
    if (any(btest(arr2, arr1) .neqv. [.true., .true., .true.])) error stop

    res = btest(arr1, arr3)
    print *, res
    if (any(res .neqv. [.false., .false., .false.])) error stop

end program 