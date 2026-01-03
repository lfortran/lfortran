program main
    integer :: a, b, c(5), d
    common / block_1 / a, b, c
    a = 1
    b = 2
    c = 3
    d = 4
    b = c(1)
    d = a + b
    a = 5 * b

    if (a > b) then
        d = a + b / d
    else if (c(1) > d) then
        d = a - b
    end if

    select case (a)
        case (1)
            d = a + b
        case (2)
            d = a - b
        case default
            d = a * b
    end select

    do while (a > b)
        d = a + b
        a = a - 1
    end do
    do a = 1, c(1), 1
        d = a + b
    end do
    print *, a, b, c, d
    if (a /= 4) error stop
    if (b /= 3) error stop
    do a = 1, 5
        if (c(a) /= 3) error stop
    end do
    if (d /= 6) error stop
end program