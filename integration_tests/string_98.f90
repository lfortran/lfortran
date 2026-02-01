! Test for https://github.com/lfortran/lfortran/issues/4888
! Test for https://github.com/lfortran/lfortran/issues/4889
! Character array concatenation in write statement
program string_98
    implicit none
    character(3) :: c1(6) = 'ab'
    character :: c2(6)*3 = 'ab'
    character(100) :: line
    integer :: i

    do i = 1, 6
        if (c1(i) /= 'ab ') error stop
        if (len(c1(i)) /= 3) error stop
        if (c2(i) /= 'ab ') error stop
        if (len(c2(i)) /= 3) error stop
    end do

    write(line, "(6A)") c1//' '
    if (trim(line) /= 'ab  ab  ab  ab  ab  ab') error stop

    write(line, "(*(A))") 'c = "', c2, '"'
    if (trim(line) /= 'c = "ab ab ab ab ab ab "') error stop

    print *, "PASS"
end program string_98
