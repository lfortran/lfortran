program intrinsics_155
    integer:: k1 = 10
    integer:: k2 = 5
    integer:: k3 = -5
    integer:: k4 = 15
    integer:: k5 = 0
    integer:: k6 = -15
    logical:: t = .true.

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
end