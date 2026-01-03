program intrinsics_285
    use iso_fortran_env, only: sp => real32, dp => real64
    implicit none
    
    real(4), parameter :: r1 = real(15)
    real(4), parameter :: r2 = real(15.12_sp)
    real(4), parameter :: r3 = real((15.123_sp, -7.0_sp))
    real(8), parameter :: r4 = real(-7, 8)
    real(8), parameter :: r5 = real(-15.123_sp, 8)
    real(8), parameter :: r6 = real(15.123_sp, 8)

    real(4), parameter :: ar1(3) = real([1, 7, 13])
    real(8), parameter :: ar2(3) = real([1.789_sp, 7.123_sp, 13.123_sp], 8)
    real(4), parameter :: ar3(3) = real([(1.789_sp, 7.123_sp), (13.123_sp, -7.0_sp), (15.123_sp, 8_sp)])

    integer(4) :: i4 = 15
    real(4) :: r = 15.123_sp
    complex(4) :: c4 = (15.123_sp, -7.0_sp)
    integer(8) :: i8 = -7
    real(8) :: r8 = -15.123_sp
    complex(8) :: c8 = (15.123_sp, 8_sp)

    integer(4) :: arr1(3)
    real(8) :: arr2(3)
    complex(4) :: arr3(3)

    arr1 = [1, 7, 13]
    arr2 = [1.789_dp, 7.123_dp, 13.123_dp]
    arr3 = [(1.789_sp, 7.123_sp), (13.123_sp, -7.0_sp), (15.123_sp, 8_sp)]

    print*, r1
    if (abs(r1 - 15.0_sp) > 1e-6_sp) error stop
    print*, r2
    if (abs(r2 - 15.12_sp) > 1e-6_sp) error stop
    print*, r3
    if (abs(r3 - 15.123_sp) > 1e-6_sp) error stop
    print*, r4
    if (abs(r4 - (-7.0_dp)) > 1e-6_dp) error stop
    print*, r5
    if (abs(r5 - (-15.123_dp)) > 1e-6_dp) error stop
    print*, r6
    if (abs(r6 - 15.123_dp) > 1e-6_dp) error stop

    print*, ar1
    if (any(abs(ar1 - [1.0_sp, 7.0_sp, 13.0_sp]) > 1e-6_sp)) error stop
    print*, ar2
    if (any(abs(ar2 - [1.789_dp, 7.123_dp, 13.123_dp]) > 1e-6_dp)) error stop
    print*, ar3
    if (any(abs(ar3 - [1.78900003_sp, 13.1230001_sp, 15.1230001_sp]) > 1e-6_sp)) error stop

    print*, real(i4)
    if (abs(real(i4) - 15.0_sp) > 1e-6_sp) error stop
    print*, real(r)
    if (abs(real(r) - 15.123_sp) > 1e-6_sp) error stop
    print*, real(c4)
    if (abs(real(c4) - 15.123_sp) > 1e-6_sp) error stop
    print*, real(i8)
    if (abs(real(i8) - (-7.0_dp)) > 1e-6_dp) error stop
    print*, real(r8)
    if (abs(real(r8) - (-15.123_dp)) > 1e-6_dp) error stop
    print*, real(c8)
    if (abs(real(c8) - 15.123_dp) > 1e-6_dp) error stop

    print*, real(arr1)
    if (any(abs(real(arr1) - [1.0_sp, 7.0_sp, 13.0_sp]) > 1e-6_sp)) error stop
    print*, real(arr2)
    if (any(abs(real(arr2) - [1.789_dp, 7.123_dp, 13.123_dp]) > 1e-6_dp)) error stop
    print*, real(arr3)
    if (any(abs(real(arr3) - [1.78900003_sp, 13.1230001_sp, 15.1230001_sp]) > 1e-6_sp)) error stop

end program