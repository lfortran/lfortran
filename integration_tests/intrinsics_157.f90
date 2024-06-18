program intrinsics_157
    logical, parameter :: l1 = ble(10, 5)
    logical, parameter :: l2 = ble(10_8, -5_8)
    integer:: k1 = 10
    integer:: k2 = 5
    integer:: k3 = -5
    integer:: k4 = 15
    integer:: k5 = 0
    integer:: k6 = -15
    logical:: f = .false.
    logical, parameter :: ar1(4) = ble([1, 2, 4, 5], 3)
    logical, parameter :: ar2(4) = ble([-1, 3, -7, 5], -3)

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

    print*, ble(10, 5)
    if (ble(10, 5) .neqv. .false.) error stop
    print*, ble(-5, 10)
    if (ble(-5, 10) .neqv. .false.) error stop
    print*, ble(-5, 5)
    if (ble(-5, 5) .neqv. .false.) error stop
    print*, ble(-5, -15)
    if (ble(-5, -15) .neqv. .false.) error stop
    print*, ble(5, 15)
    if (ble(5, 15) .neqv. .true.) error stop
    print*, ble(5, -15)
    if (ble(5, -15) .neqv. .true.) error stop
    print*, ble(0, -5)
    if (ble(0, -5) .neqv. .true.) error stop
    print*, ble(0, 5)
    if (ble(0, 5) .neqv. .true.) error stop
    print*, ble(-5, 0)
    if (ble(-5, 0) .neqv. .false.) error stop
    print*, ble(5, 0)
    if (ble(5, 0) .neqv. f) error stop

    print*, ble(k1, k2)
    if (ble(k1, k2) .neqv. .false.) error stop
    print*, ble(k3, k1)
    if (ble(k3, k1) .neqv. .false.) error stop
    print*, ble(k3, k3)
    if (ble(k3, k3) .neqv. .true.) error stop
    print*, ble(k3, k6)
    if (ble(k3, k6) .neqv. .false.) error stop
    print*, ble(k2, k4)
    if (ble(k2, k4) .neqv. .true.) error stop
    print*, ble(k2, k6)
    if (ble(k2, k6) .neqv. .true.) error stop
    print*, ble(k5, k3)
    if (ble(k5, k3) .neqv. .true.) error stop
    print*, ble(k5, k2)
    if (ble(k5, k2) .neqv. .true.) error stop
    print*, ble(k3, k5)
    if (ble(k3, k5) .neqv. .false.) error stop
    print*, ble(k2, k5)
    if (ble(k2, k5) .neqv. f) error stop

    print *, ble(arr1, 5)
    if (any(ble(arr1, 5) .neqv. .true.) .neqv. .true.) error stop

    res = ble(arr2, k5)
    print *, res
    if (any(res .neqv. .true.) .neqv. .true.) error stop

end program
