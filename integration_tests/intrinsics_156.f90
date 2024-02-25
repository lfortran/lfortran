program intrinsics_156
    integer:: k1 = 10
    integer:: k2 = 5
    integer:: k3 = -5
    integer:: k4 = 15
    integer:: k5 = 0
    integer:: k6 = -15
    logical:: f = .false.

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
end
