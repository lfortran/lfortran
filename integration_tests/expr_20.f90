program expr_20

    integer :: n
    n = -123
    n = n / 10
    print *, n

    if (n /= -12) error stop
end program
