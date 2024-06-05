program intrinsics_244
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    real(dp) :: a, b
    real(sp) :: e, f
    integer :: i 
    real(dp) :: runtime_dp(2) = [62.6372_dp, -72.526_dp]
    real(sp) :: runtime_sp(2) = [35.637_sp, -41.627_sp]
    real(dp) :: sin_res_dp(2), cos_res_dp(2), tan_res_dp(2)
    real(sp) :: sin_res_sp(2), cos_res_sp(2), tan_res_sp(2)
    real(dp) :: expected1_dp(2) = [-0.19342617020229108_dp, 0.26612321280164114_dp]
    real(dp) :: expected2_dp(2) = [0.98111483358517937_dp, -0.96393902068965565_dp]
    real(dp) :: expected3_dp(2) = [-0.19714936884143847_dp, -0.27607888786496237_dp]
    real(sp) :: expected1_sp(2) = [-0.881713450, 0.707740247]
    real(sp) :: expected2_sp(2) = [-0.471785307, -0.706472754]
    real(sp) :: expected3_sp(2) = [1.86888719, -1.00179410]

    real(dp), parameter :: res1(2) = sin([62.6372_dp, -72.526_dp])
    real(dp), parameter :: res2(2) = cos([62.6372_dp, -72.526_dp])
    real(dp), parameter :: res3(2) = tan([62.6372_dp, -72.526_dp])
    real(sp), parameter :: res1_sp(2) = sin([35.637_sp, -41.627_sp])
    real(sp), parameter :: res2_sp(2) = cos([35.637_sp, -41.627_sp])
    real(sp), parameter :: res3_sp(2) = tan([35.637_sp, -41.627_sp])

    do i = 1, 2
        print *, res1(i)
        if (abs(res1(i) - expected1_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 2
        print *, res2(i)
        if (abs(res2(i) - expected2_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 2
        print *, res3(i)
        if (abs(res3(i) - expected3_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 2
        print *, res1_sp(i)
        if (abs(res1_sp(i) - expected1_sp(i)) > 1e-5) error stop
    end do

    do i = 1, 2
        print *, res2_sp(i)
        if (abs(res2_sp(i) - expected2_sp(i)) > 1e-5) error stop
    end do

    do i = 1, 2
        print *, res3_sp(i)
        if (abs(res3_sp(i) - expected3_sp(i)) > 1e-5) error stop
    end do

    sin_res_dp = sin(runtime_dp)
    cos_res_dp = cos(runtime_dp)
    tan_res_dp = tan(runtime_dp)
    sin_res_sp = sin(runtime_sp)
    cos_res_sp = cos(runtime_sp)
    tan_res_sp = tan(runtime_sp)

    do i = 1, 2
        print *, sin_res_dp(i)
        if (abs(sin_res_dp(i) - expected1_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 2
        print *, cos_res_dp(i)
        if (abs(cos_res_dp(i) - expected2_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 2
        print *, tan_res_dp(i)
        if (abs(tan_res_dp(i) - expected3_dp(i)) > 1e-12_dp) error stop
    end do

    do i = 1, 2
        print *, sin_res_sp(i)
        if (abs(sin_res_sp(i) - expected1_sp(i)) > 1e-5) error stop
    end do

    do i = 1, 2
        print *, cos_res_sp(i)
        if (abs(cos_res_sp(i) - expected2_sp(i)) > 1e-5) error stop
    end do

    do i = 1, 2
        print *, tan_res_sp(i)
        if (abs(tan_res_sp(i) - expected3_sp(i)) > 1e-5) error stop
    end do

    a = 62.6372_dp
    b = -72.526_dp

    e = 35.637_sp
    f = -41.627_sp

    print *, sin(a)
    if(abs(sin(a) - (-0.19342617020229108_dp)) > 1e-12) error stop

    print *, sin(b)
    if(abs(sin(b) - 0.26612321280164114_dp) > 1e-12) error stop

    print *, cos(a)
    if(abs(cos(a) - 0.98111483358517937_dp) > 1e-12) error stop

    print *, cos(b)
    if(abs(cos(b) - (-0.96393902068965565_dp)) > 1e-12) error stop

    print *, tan(a)
    if(abs(tan(a) - (-0.19714936884143847_dp)) > 1e-12) error stop

    print *, tan(b)
    if(abs(tan(b) - (-0.27607888786496237_dp)) > 1e-12) error stop

    print *, sin(e)
    if(abs(sin(e) - (-0.881713450)) > 1e-6) error stop

    print *, sin(f)
    if(abs(sin(f) - 0.707740247) > 1e-6) error stop

    print *, cos(e)
    if(abs(cos(e) - (-0.471785307)) > 1e-6) error stop

    print *, cos(f)
    if(abs(cos(f) - (-0.706472754)) > 1e-6) error stop

    print *, tan(e)
    if(abs(tan(e) - 1.86888719) > 1e-6) error stop

    print *, tan(f)
    if(abs(tan(f) - (-1.00179410)) > 1e-6) error stop

end program 