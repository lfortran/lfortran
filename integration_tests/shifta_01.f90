program shifta_01
    implicit none
    integer(kind=4) :: n, x, result

    ! Test case 1: shift by 0 bits
    n = 12345678
    x = 0
    result = shifta(n, x)
    print *, "Test case 1: shifta(", n, ",", x, ") = ", result
    if (result /= n) error stop "Test case 1 failed"

    ! Test case 2: shift by 1 bit
    n = 12345678
    x = 1
    result = shifta(n, x)
    print *, "Test case 2: shifta(", n, ",", x, ") = ", result
    if (result /= 6172839) error stop "Test case 2 failed"

    ! Test case 3: shift by 15 bits
    n = 12345678
    x = 15
    result = shifta(n, x)
    print *, "Test case 3: shifta(", n, ",", x, ") = ", result
    if (result /= 376) error stop "Test case 3 failed"

    ! Test case 4: shift by 16 bits
    n = 12345678
    x = 16
    result = shifta(n, x)
    print *, "Test case 4: shifta(", n, ",", x, ") = ", result
    if (result /= 188) error stop "Test case 4 failed"

    ! Test case 5: shift by 30 bits
    n = 2**29
    x = 30
    result = shifta(n, x)
    print *, "Test case 5: shifta(", n, ",", x, ") = ", result
    if (result /= 0) error stop "Test case 5 failed"

    n = 2**30
    x = 30
    result = shifta(n, x)
    print *, "Test case 5: shifta(", n, ",", x, ") = ", result
    if (result /= 1) error stop "Test case 5 failed"

    ! Test case 6: shift by 31 bits
    n = 2**29 +  3
    x = 31
    result = shifta(n, x)
    print *, "Test case 6: shifta(", n, ",", x, ") = ", result
    if (result /= 0) error stop "Test case 6 failed"

end program shifta_01
