program intrinsics_240
    integer(4) :: i, j, o, p
    integer(8) :: k, l, m, n
    integer(4) :: shift, shift2
    integer(8) :: shift3, shift1

    integer(8) :: expected_run(2)
    integer(4) :: expected_comp_sp(2)
    integer(8) :: expected_comp_dp(2)

    integer(8) :: first_arg(2) = [7, -10]
    integer(8) :: second_arg(2) = [12, -12]
    integer(4) :: third_arg(2) = [5, 7]

    integer(8) :: dshiftr_runtime_res(2)
         
    integer(4), parameter :: res_comp_sp(2) = dshiftr([10, 632], [7, 918], [8, 17])
    integer(8), parameter :: res_comp_dp(2) = dshiftr([14526_8, -726382_8], [12345_8, 1725422_8], [31, 42])

    expected_comp_dp = [124777389883392_8, -3046666928128_8]
    expected_comp_sp = [167772160, 20709376]
    expected_run = [4035225266123964416_8, -1297036692682702849_8]

    dshiftr_runtime_res = dshiftr(first_arg, second_arg, third_arg)

    do i = 1, size(res_comp_sp)
        print *, res_comp_sp(i)
        if (res_comp_sp(i) /= expected_comp_sp(i)) error stop
    end do

    do i = 1, size(res_comp_dp)
        print *, res_comp_dp(i)
        if (res_comp_dp(i) /= expected_comp_dp(i)) error stop
    end do

    do i = 1, size(dshiftr_runtime_res)
        print *, dshiftr_runtime_res(i)
        if (dshiftr_runtime_res(i) /= expected_run(i)) error stop
    end do

    i = 10
    j = 7
    shift = 8

    k = 14526
    l = 12345
    m = -726382
    n = 1725422
    o = 632
    p = 918

    shift1 = 17
    shift2 = 31
    shift3 = 42

    print *, dshiftr(o, p, shift1)
    if (dshiftr(o, p, shift1) /= 20709376) error stop

    print *, dshiftr(k, l, shift2)
    if (dshiftr(k, l, shift2) /= 124777389883392_8) error stop

    print *, dshiftr(m, n, shift3)
    if (dshiftr(m, n, shift3) /= -3046666928128_8) error stop

    print *, dshiftr(10, 7, 6)
    if(dshiftr(10, 7, 6) /= 671088640) error stop

    print *, dshiftr(10, -12, 7)
    if(dshiftr(10, -12, 7) /= 369098751) error stop

    print *, dshiftr(7_8, 12_8, 5)
    if (dshiftr(7_8, 12_8, 5) /= 4035225266123964416_8) error stop

    print *, kind(dshiftr(-10, 4, 3))
    if(kind(dshiftr(-10, 4, 3)) /= 4) error stop

    print *, kind(dshiftr(7_8, 12_8, 5))
    if(kind(dshiftr(7_8, 12_8, 5)) /= 8) error stop

    print *, kind(dshiftr(-10, -12, 7))
    if(kind(dshiftr(10, 12, 7)) /= 4) error stop

end program 



