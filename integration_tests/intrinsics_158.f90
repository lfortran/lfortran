program intrinsics_158
    logical, parameter :: l1 = bge(10, 5)
    logical, parameter :: l2 = bge(10_8, -5_8)
    integer:: k1 = 10
    integer:: k2 = 5
    integer:: k3 = -5
    integer:: k4 = 15
    integer:: k5 = 0
    integer:: k6 = -15
    logical:: t = .true.
    logical, parameter :: ar1(4) = bge([1, 2, 4, 5], 3)
    logical, parameter :: ar2(4) = bge([-1, 3, -7, 5], -3)

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

    print*, bge(10, 5)
    if (bge(10, 5) .neqv. .true.) error stop
    print*, bge(-5, 10)
    if (bge(-5, 10) .neqv. .True.) error stop
    print*, bge(-5, 5)
    if (bge(-5, 5) .neqv. .true.) error stop
    print*, bge(-5, -15)
    if (bge(-5, -15) .neqv. .true.) error stop
    print*, bge(5, 15)
    if (bge(5, 15) .neqv. .false.) error stop
    print*, bge(5, -15)
    if (bge(5, -15) .neqv. .false.) error stop
    print*, bge(0, -5)
    if (bge(0, -5) .neqv. .false.) error stop
    print*, bge(0, 5)
    if (bge(0, 5) .neqv. .false.) error stop
    print*, bge(-5, 0)
    if (bge(-5, 0) .neqv. .true.) error stop
    print*, bge(5, 0)
    if (bge(5, 0) .neqv. t) error stop

    print*, bge(k1, k2)
    if (bge(k1, k2) .neqv. .true.) error stop
    print*, bge(k3, k1)
    if (bge(k3, k1) .neqv. .true.) error stop
    print*, bge(k3, k3)
    if (bge(k3, k3) .neqv. .true.) error stop
    print*, bge(k3, k6)
    if (bge(k3, k6) .neqv. .true.) error stop
    print*, bge(k2, k4)
    if (bge(k2, k4) .neqv. .false.) error stop
    print*, bge(k2, k6)
    if (bge(k2, k6) .neqv. .false.) error stop
    print*, bge(k5, k3)
    if (bge(k5, k3) .neqv. .false.) error stop
    print*, bge(k5, k2)
    if (bge(k5, k2) .neqv. .false.) error stop
    print*, bge(k3, k5)
    if (bge(k3, k5) .neqv. .true.) error stop
    print*, bge(k2, k5)
    if (bge(k2, k5) .neqv. t) error stop

    print *, bge(arr1, 5)
    if (any(bge(arr1, 5) .neqv. .true.) .neqv. .true.) error stop

    res = bge(arr2, k5)
    print *, res
    if (any(res .neqv. .true.) .neqv. .false.) error stop

end program