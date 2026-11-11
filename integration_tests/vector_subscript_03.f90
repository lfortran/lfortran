program vector_subscript_03
    implicit none
    integer :: irow(2), idx(2)
    real :: df(3), dg(3)

    idx = [2, 1]
    irow = [3, 1]
    df = [4.0, 8.0, 12.0]
    ! irow(idx) is [1, 3]
    df(irow(idx)) = df(irow(idx)) / 2.0
    if (abs(df(1) - 2.0) > 1.0e-6) error stop
    if (abs(df(2) - 8.0) > 1.0e-6) error stop
    if (abs(df(3) - 6.0) > 1.0e-6) error stop

    dg = [4.0, 8.0, 12.0]
    associate (rows => irow(idx))
        dg(rows) = dg(rows) / 2.0
    end associate
    if (abs(dg(1) - 2.0) > 1.0e-6) error stop
    if (abs(dg(2) - 8.0) > 1.0e-6) error stop
    if (abs(dg(3) - 6.0) > 1.0e-6) error stop

    dg = [4.0, 8.0, 12.0]
    associate (rows => irow(idx(2:2)))
        dg(rows) = dg(rows) / 2.0
    end associate
    ! idx(2:2) is [1], irow(1) is 3
    if (abs(dg(1) - 4.0) > 1.0e-6) error stop
    if (abs(dg(2) - 8.0) > 1.0e-6) error stop
    if (abs(dg(3) - 6.0) > 1.0e-6) error stop
end program
