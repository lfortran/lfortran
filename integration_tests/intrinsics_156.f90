program intrinsics_156
    logical, parameter :: l1 = blt(10, 5)
    logical, parameter :: l2 = blt(10_8, -5_8)
    integer:: k1 = 10
    integer:: k2 = 5
    integer:: k3 = -5
    integer:: k4 = 15
    integer:: k5 = 0
    integer:: k6 = -15
    logical:: f = .false.
    logical, parameter :: ar1(4) = blt([1, 2, 4, 5], 3)
    logical, parameter :: ar2(4) = blt([-1, 3, -7, 5], -3)

    integer :: arr1(4)
    integer :: arr2(3)
    logical :: res(3)

    arr1 = [11, 2, 13, 4]
    arr2 = [-5_8, 7_8, 0_8]

    print *, l1
    if (l1 .neqv. .false.) error stop

    print *, l2
    if (l2 .neqv. .true.) error stop

    print *, ar1
    if (any(ar1) .neqv. .true.) error stop

    print *, ar2
    if (any(ar2) .neqv. .true.) error stop

    print*, blt(10, 5)
    if (blt(10, 5) .neqv. .false.) error stop
    print*, blt(-5, 10)
    if (blt(-5, 10) .neqv. .false.) error stop
    print*, blt(-5, 5)
    if (blt(-5, 5) .neqv. .false.) error stop
    print*, blt(-5, -15)
    if (blt(-5, -15) .neqv. .false.) error stop
    print*, blt(5, 15)
    if (blt(5, 15) .neqv. .true.) error stop
    print*, blt(5, -15)
    if (blt(5, -15) .neqv. .true.) error stop
    print*, blt(0, -5)
    if (blt(0, -5) .neqv. .true.) error stop
    print*, blt(0, 5)
    if (blt(0, 5) .neqv. .true.) error stop
    print*, blt(-5, 0)
    if (blt(-5, 0) .neqv. .false.) error stop
    print*, blt(5, 0)
    if (blt(5, 0) .neqv. f) error stop

    print*, blt(k1, k2)
    if (blt(k1, k2) .neqv. .false.) error stop
    print*, blt(k3, k1)
    if (blt(k3, k1) .neqv. .false.) error stop
    print*, blt(k3, k3)
    if (blt(k3, k3) .neqv. .false.) error stop
    print*, blt(k3, k6)
    if (blt(k3, k6) .neqv. .false.) error stop
    print*, blt(k2, k4)
    if (blt(k2, k4) .neqv. .true.) error stop
    print*, blt(k2, k6)
    if (blt(k2, k6) .neqv. .true.) error stop
    print*, blt(k5, k3)
    if (blt(k5, k3) .neqv. .true.) error stop
    print*, blt(k5, k2)
    if (blt(k5, k2) .neqv. .true.) error stop
    print*, blt(k3, k5)
    if (blt(k3, k5) .neqv. .false.) error stop
    print*, blt(k2, k5)
    if (blt(k2, k5) .neqv. f) error stop

    print *, blt(arr1, 5)
    if (any(blt(arr1, 5) .neqv. .true.) .neqv. .true.) error stop

    res = blt(arr2, k5)
    print *, res
    if (any(res .neqv. .true.) .neqv. .true.) error stop

end program
