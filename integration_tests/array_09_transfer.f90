! Test for https://github.com/lfortran/lfortran/issues/7049
! transfer string to character array
program array_09_transfer
    implicit none
    character(1) :: result(3)
    result = transfer('abc', ['x'])
    if (result(1) /= 'a') error stop
    if (result(2) /= 'b') error stop
    if (result(3) /= 'c') error stop
    print "(3(1X,A))", result
end program array_09_transfer
