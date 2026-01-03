program intrinsic_150
    integer :: a,b
    integer(8) :: c, d
    integer(8) :: result
    integer :: i

    integer(4) :: arg_x(5) = [12, 11, 18, 20, 30]
    integer(8) :: arg_x2(5) = [33, 48, 58, 55, 54] 
    integer(4) :: res_x(5)
    integer(8) :: res_x2(5)

    integer(4), parameter :: res(5) = maskr([12, 11, 18, 20, 30])
    integer(8), parameter :: res2(5) = maskr([33, 48, 58, 55, 54],8)

    integer(4) :: expected_res(5) = [4095,  2047, 262143, 1048575, 1073741823]
    integer(8) :: expected_res2(5) = [8589934591_8, 281474976710655_8, 288230376151711743_8, &
            36028797018963967_8, 18014398509481983_8]

    integer(4), parameter :: comp1 = maskr(12)
    integer(8), parameter :: comp2 = maskr(43, 8)

    print *, comp1
    if (comp1 /= 4095) error stop

    print *, comp2
    if (comp2 /= 8796093022207_8) error stop

    res_x = maskr(arg_x)
    res_x2 = maskr(arg_x2, 8)

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
        ! if (res_x2(i) /= expected_res2(i)) error stop
    end do

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
