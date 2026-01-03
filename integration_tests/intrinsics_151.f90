program intrinsics_151
    integer :: a,b
    integer(8) :: c, d
    integer(8) :: result
    integer :: i

    integer(4) :: arg_x(5) = [12, 11, 18, 20, 30]
    integer(8) :: arg_x2(5) = [33, 48, 58, 55, 54] 
    integer(4) :: res_x(5)
    integer(8) :: res_x2(5)

    integer(4), parameter :: res(5) = maskl([12, 11, 18, 20, 30])
    integer(8), parameter :: res2(5) = maskl([33, 48, 58, 55, 54],8)

    integer(4) :: expected_res(5) = [-1048576,  -2097152, -16384, -4096, -4]
    integer(8) :: expected_res2(5) = [-2147483648_8, -65536_8, -64_8, &
            -512_8, -1024_8]

    integer(4), parameter :: comp1 = maskl(12)
    integer(8), parameter :: comp2 = maskl(43, 8)

    print *, comp1
    if (comp1 /= -1048576) error stop

    print *, comp2
    if (comp2 /= -2097152_8) error stop

    res_x = maskl(arg_x)
    res_x2 = maskl(arg_x2, 8)

    do i = 1, size(res)
        print *, res(i)
        if (res(i) /= expected_res(i)) error stop
    end do

    do i = 1, size(res2)
        print *, res2(i)
        if (res2(i) /= expected_res2(i)) error stop
    end do

    do i = 1, size(res_x)
        print *, res_x(i)
        if (res_x(i) /= expected_res(i)) error stop
    end do

    do i = 1, size(res_x2)
        print *, res_x2(i)
        if (res_x2(i) /= expected_res2(i)) error stop
    end do

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

    print*, maskl(55, kind = kind(0.0d0))
    if (maskl(55, kind = kind(0.0d0)) /= -512) error stop

    print *, kind(maskl(10,8))
    if (kind(maskl(10,8)) /= 8) error stop

    print *, kind(maskl(10))
    if (kind(maskl(10)) /= 4) error stop

    print *, kind(maskl(a,4))
    if (kind(maskl(a,4)) /= 4) error stop

    print *, kind(maskl(a,8))
    if (kind(maskl(a,8)) /= 8) error stop

end program
