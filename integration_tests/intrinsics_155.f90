program intrinsics_155
    logical, parameter :: l1 = bgt(10, 5)
    logical, parameter :: l2 = bgt(10_8, -5_8)
    integer :: k1 = 10
    integer :: k2 = 5
    integer :: k3 = -5
    integer :: k4 = 15
    integer :: k5 = 0
    integer :: k6 = -15
    logical :: t = .true.
    logical, parameter :: ar1(4) = bgt([1, 2, 4, 5], 3)
    logical, parameter :: ar2(4) = bgt([-1, 3, -7, 5], -3)
    integer :: arr1(4)
    integer :: arr2(3)
    logical :: res(3)

    arr1 = [11, 2, 13, 4]
    arr2 = [-5_8, 7_8, 0_8]

    print *, l1
    if (l1 .neqv. .true.) error stop

    print *, l2
    if (l2 .neqv. .false.) error stop

    print *, ar1
    if (any(ar1) .neqv. .true.) error stop

    print *, ar2
    if (any(ar2) .neqv. .true.) error stop

    print*, bgt(10, 5)
    if (bgt(10, 5) .neqv. .true.) error stop
    print*, bgt(-5, 10)
    if (bgt(-5, 10) .neqv. .true.) error stop
    print*, bgt(-5, 5)
    if (bgt(-5, 5) .neqv. .true.) error stop
    print*, bgt(-5, -15)
    if (bgt(-5, -15) .neqv. .true.) error stop
    print*, bgt(5, 15)
    if (bgt(5, 15) .neqv. .false.) error stop
    print*, bgt(5, -15)
    if (bgt(5, -15) .neqv. .false.) error stop
    print*, bgt(0, -5)
    if (bgt(0, -5) .neqv. .false.) error stop
    print*, bgt(0, 5)
    if (bgt(0, 5) .neqv. .false.) error stop
    print*, bgt(-5, 0)
    if (bgt(-5, 0) .neqv. .true.) error stop
    print*, bgt(5, 0)
    if (bgt(5, 0) .neqv. t) error stop

    print*, bgt(k1, k2)
    if (bgt(k1, k2) .neqv. .true.) error stop
    print*, bgt(k3, k1)
    if (bgt(k3, k1) .neqv. .true.) error stop
    print*, bgt(k3, k3)
    if (bgt(k3, k3) .neqv. .false.) error stop
    print*, bgt(k3, k6)
    if (bgt(k3, k6) .neqv. .true.) error stop
    print*, bgt(k2, k4)
    if (bgt(k2, k4) .neqv. .false.) error stop
    print*, bgt(k2, k6)
    if (bgt(k2, k6) .neqv. .false.) error stop
    print*, bgt(k5, k3)
    if (bgt(k5, k3) .neqv. .false.) error stop
    print*, bgt(k5, k2)
    if (bgt(k5, k2) .neqv. .false.) error stop
    print*, bgt(k3, k5)
    if (bgt(k3, k5) .neqv. .true.) error stop
    print*, bgt(k2, k5)
    if (bgt(k2, k5) .neqv. t) error stop

    print *, bgt(arr1, 5)
    if (any(bgt(arr1, 5) .neqv. .true.) .neqv. .true.) error stop

    res = bgt(arr2, k5)
    print *, res
    if (any(res .neqv. .true.) .neqv. .true.) error stop

end program