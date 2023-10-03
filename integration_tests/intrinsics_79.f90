program intrinsics_79
    implicit none
    integer :: x, result

    ! Test case 1: x = 0
    x = 0
    result = trailz(x)
    print *, "Test case 1: trailz(", x, ") = ", result
    if (result /= 32) error stop "Test case 1 failed"

    ! Test case 2: x = 1
    x = 1
    result = trailz(x)
    print *, "Test case 2: trailz(", x, ") = ", result
    if (result /= 0) error stop "Test case 2 failed"

    ! Test case 3: x = 2
    x = 2
    result = trailz(x)
    print *, "Test case 3: trailz(", x, ") = ", result
    if (result /= 1) error stop "Test case 3 failed"

    ! Test case 4: x = 3
    x = 3
    result = trailz(x)
    print *, "Test case 4: trailz(", x, ") = ", result
    if (result /= 0) error stop "Test case 4 failed"

    ! Test case 5: x = 4
    x = 4
    result = trailz(x)
    print *, "Test case 5: trailz(", x, ") = ", result
    if (result /= 2) error stop "Test case 5 failed"

    ! Test case 6: x = 8
    x = 8
    result = trailz(x)
    print *, "Test case 6: trailz(", x, ") = ", result
    if (result /= 3) error stop "Test case 6 failed"

    ! Test case 7: x = 16
    x = 16
    result = trailz(x)
    print *, "Test case 7: trailz(", x, ") = ", result
    if (result /= 4) error stop "Test case 7 failed"

    ! Test case 8: x = 2147483647 (maximum 32-bit integer)
    x = 2147483647
    result = trailz(x)
    print *, "Test case 8: trailz(", x, ") = ", result
    if (result /= 0) error stop "Test case 8 failed"

    ! Test case 9: x = -1 (all bits set)
    x = -1
    result = trailz(x)
    print *, "Test case 9: trailz(", x, ") = ", result
    if (result /= 0) error stop "Test case 9 failed"

    ! Test case 10: x = -2147483648 (minimum 32-bit integer)
    x = -2147483647
    result = trailz(x)
    print *, "Test case 10: trailz(", x, ") = ", result
    if (result /= 0) error stop "Test case 10 failed"

    ! Test case 11: x = 1431655765 (1010101010101010101010101010101 in binary)
    x = 1431655765
    result = trailz(x)
    print *, "Test case 11: trailz(", x, ") = ", result
    if (result /= 0) error stop "Test case 11 failed"
end program intrinsics_79
