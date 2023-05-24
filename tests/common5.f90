program main
    integer :: a, b, c(5), d
    common / block_1 / a, b, c
    b = c(1)
    d = a + b * c
    a = 5 * b

    if (a > b) then
        d = a + b / d
    else if (c(1) > d) then
        d = a - b
    end if

    where (c > 10)
        d = a + b
    end where

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

    do a = 1, c(1), d
        d = a + b
    end do
end program