program intrinsics_245
    complex(8) :: a, b, c
    complex(4) :: x, y, z
    real(8) :: d, e, f
    real(4) :: o, p, q
    integer(8) :: i, j, k
    integer(4) :: l, m, n

    integer(4) :: res_sp(3) = [1,2,3]
    integer(8) :: res_dp(3) = [1,2,3]

    integer(4) :: run_complex_dp(3)
    integer(4) :: run_complex_sp(3)
    integer(4) :: run_real_dp(3)
    integer(4) :: run_real_sp(3)
    integer(4) :: run_integer_dp(3)
    integer(4) :: run_integer_sp(3)

    integer(8), parameter :: complex_dp(3) = int([(1.0_8, 2.0_8), (2.0_8, 4.0_8), (3.0_8, 2.0_8)])
    integer(4), parameter :: complex_sp(3) = int([(1.0_4, 2.0_4), (2.0_4, 4.0_4), (3.0_4, 2.0_4)])
    integer(8), parameter :: real_dp(3) = int([1.0_8, 2.0_8, 3.0_8])
    integer(4), parameter :: real_sp(3) = int([1.0_4, 2.0_4, 3.0_4])
    integer(8), parameter :: integer_dp(3) = int([1, 2, 3])
    integer(4), parameter :: integer_sp(3) = int([1, 2, 3])

    do i = 1, size(complex_dp)
        print *, complex_dp(i)
        if (complex_dp(i) /= res_dp(i)) error stop
    end do

    do i = 1, size(complex_sp)
        print *, complex_sp(i)
        if (complex_sp(i) /= res_sp(i)) error stop
    end do

    do i = 1, size(real_dp)
        print *, real_dp(i)
        if (real_dp(i) /= res_dp(i)) error stop
    end do

    do i = 1, size(real_sp)
        print *, real_sp(i)
        if (real_sp(i) /= res_sp(i)) error stop
    end do

    do i = 1, size(integer_dp)
        print *, integer_dp(i)
        if (integer_dp(i) /= res_dp(i)) error stop
    end do

    do i = 1, size(integer_sp)
        print *, integer_sp(i)
        if (integer_sp(i) /= res_sp(i)) error stop
    end do

    a = (1.0, 2.0)
    b = (2.0, 4.0)
    c = (3.0, 2.0)
    x = (1.0, 2.0)
    y = (2.0, 4.0)
    z = (3.0, 2.0)
    run_complex_dp = int([a, b, c])
    run_complex_sp = int([x, y, z])

    do i = 1, size(run_complex_dp)
        print *, run_complex_dp(i)
        if (run_complex_dp(i) /= res_dp(i)) error stop
    end do

    do i = 1, size(run_complex_sp)
        print *, run_complex_sp(i)
        if (run_complex_sp(i) /= res_sp(i)) error stop
    end do

    d = 1.0_8
    e = 2.0_8
    f = 3.0_8
    o = 1.0_4
    p = 2.0_4
    q = 3.0_4
    run_real_dp = int([d, e, f])
    run_real_sp = int([o, p, q])

    do i = 1, size(run_real_dp)
        print *, run_real_dp(i)
        if (run_real_dp(i) /= res_dp(i)) error stop
    end do

    do i = 1, size(run_real_sp)
        print *, run_real_sp(i)
        if (run_real_sp(i) /= res_sp(i)) error stop
    end do

    i = 1
    j = 2
    k = 3
    l = 1
    m = 2
    n = 3
    run_integer_dp = int([i, j, k])
    run_integer_sp = int([l, m, n])

    do i = 1, size(run_integer_dp)
        print *, run_integer_dp(i)
        if (run_integer_dp(i) /= res_dp(i)) error stop
    end do

    do i = 1, size(run_integer_sp)
        print *, run_integer_sp(i)
        if (run_integer_sp(i) /= res_sp(i)) error stop
    end do

    print *, int(a)
    if (int(a) /= 1) error stop

    print *, int(x)
    if (int(x) /= 1) error stop

    print *, int(d)
    if (int(d) /= 1) error stop

    print *, int(o)
    if (int(o) /= 1) error stop

    print *, int(l)
    if (int(l) /= 1) error stop

end program