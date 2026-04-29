program format_90
    implicit none
    character(3) :: fmt_arr(2)
    character(5) :: res

    ! Test: character array used as FMT= keyword argument
    ! The format string is formed by concatenating all array elements.
    fmt_arr(1) = '(A'
    fmt_arr(2) = '5)'
    write(unit=res, fmt=fmt_arr) 'hello'
    if (res /= 'hello') error stop

    ! Also test positional syntax (should work the same)
    res = '     '
    write(res, fmt_arr) 'world'
    if (res /= 'world') error stop

    print *, 'PASS'
end program
