program intrinsics_155
    integer:: k1 = 10
    integer:: k2 = 5
    integer:: k3 = -5
    integer:: k4 = 15
    integer:: k5 = 0
    integer:: k6 = -15
    logical:: t = .true.

    print*, bgt(10, 5)
    if (bgt(10, 5) .neqv. .true.) error stop
    print*, bgt(-5, 10)
    if (bgt(-5, 10) .neqv. .True.) error stop
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
end