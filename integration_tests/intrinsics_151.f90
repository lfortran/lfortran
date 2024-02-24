program intrinsics_151
    integer :: a,b
    integer(8) :: c, d
    integer(8) :: result

    a = 10
    b = 31
    c = 63
    d = 54
    result = maskl(10, 8)
    print *, result
    if (result /= -18014398509481984_8) error stop
    result = maskl(31, 8)
    print *, result
    if (result /= -8589934592_8) error stop

    result = maskl(63, 8)
    print *, result
    if (result /= -2) error stop

    result = maskl(54, 8)
    print *, result
    if (result /= -1024) error stop

    result = maskl(a, 8)
    print *, result
    if (result /= -18014398509481984_8) error stop

    result = maskl(b, 8)
    print *, result
    if (result /= -8589934592_8) error stop

    result = maskl(c)
    print *, result
    if (result /= -2) error stop

    result = maskl(d)
    print *, result
    if (result /= -1024) error stop

    result = maskl(10,8)
    print *, kind(result)
    if (kind(result) /= 8) error stop

    print *, kind(maskl(10,8))
    if (kind(maskl(10,8)) /= 8) error stop

    print *, kind(maskl(10))
    if (kind(maskl(10)) /= 4) error stop

    print *, kind(maskl(a,4))
    if (kind(maskl(a,4)) /= 4) error stop

    print *, kind(maskl(a,8))
    if (kind(maskl(a,8)) /= 8) error stop

end program
