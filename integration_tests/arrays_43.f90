program arrays_43
    integer(8), parameter :: a(4) = [-10, 2, 3, 4]
    real(8), parameter :: x(4) = [-1.0, 2.0, -3.0, 4.0]
    logical(4):: b(4), c(4), d(4), e(4)

    b = a > 0
    print*, a > 0
    if (b(1) .neqv. .false. ) error stop
    if (b(2) .neqv. .true. ) error stop
    if (b(3) .neqv. .true. ) error stop
    if (b(4) .neqv. .true. ) error stop

    c = a < 0
    print*, a < 0
    if (c(1) .neqv. .true. ) error stop
    if (c(2) .neqv. .false. ) error stop
    if (c(3) .neqv. .false. ) error stop
    if (c(4) .neqv. .false. ) error stop

    d = x > 0.0
    print*, x > 0.0
    if (d(1) .neqv. .false. ) error stop
    if (d(2) .neqv. .true. ) error stop
    if (d(3) .neqv. .false. ) error stop
    if (d(4) .neqv. .true. ) error stop

    e = x < 0.0
    print*, x < 0.0
    if (e(1) .neqv. .true. ) error stop
    if (e(2) .neqv. .false. ) error stop
    if (e(3) .neqv. .true. ) error stop
    if (e(4) .neqv. .false. ) error stop
    
end program 