program intrinsic_150
    integer :: a,b
    integer(8) :: c, d
    integer(8) :: result

    a = 10
    b = 31
    c = 63
    d = 54
    result = maskr(10)
    print *, result
    if (result /= 1023) error stop
    result = maskr(31)
    print *, result
    if (result /= 2147483647) error stop

    result = maskr(63, 8)
    print *, result
    if (result /= 9223372036854775807_8) error stop

    result = maskr(54, 8)
    print *, result
    if (result /= 18014398509481983_8) error stop

    result = maskr(a)
    print *, result
    if (result /= 1023) error stop

    result = maskr(b)
    print *, result
    if (result /= 2147483647) error stop

    result = maskr(c, 8)
    print *, result
    if (result /= 9223372036854775807_8) error stop

    result = maskr(d, 8)
    print *, result
    if (result /= 18014398509481983_8) error stop

    result = maskr(10,8)
    print *, kind(result)
    if (kind(result) /= 8) error stop

    print *, kind(maskr(10,8))
    if (kind(maskr(10,8)) /= 8) error stop

    print *, kind(maskr(10))
    if (kind(maskr(10)) /= 4) error stop

    print *, kind(maskr(a,4))
    if (kind(maskr(a,4)) /= 4) error stop

    print *, kind(maskr(a,8))
    if (kind(maskr(a,8)) /= 8) error stop


end program
