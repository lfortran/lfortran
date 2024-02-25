program intrinsics_157
    integer:: k1 = 10
    integer:: k2 = 5
    integer:: k3 = -5
    integer:: k4 = 15
    integer:: k5 = 0
    integer:: k6 = -15
    logical:: f = .false.

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
end
