program complex10_param_fold_02
    ! Elemental intrinsics on complex(10) constants are folded in constant
    ! expressions
    implicit none
    complex(10), parameter :: z = (1.0_10, 1.0_10)
    complex(10), parameter :: h = (0.5_10, 0.5_10)
    complex(10), parameter :: p_exp = exp((1.0_10, 1.0_10))
    complex(10), parameter :: p_sqrt = sqrt(z)
    complex(10), parameter :: p_sqrt34 = sqrt((3.0_10, 4.0_10))
    complex(10), parameter :: p_log = log(z)
    complex(10), parameter :: p_sin = sin(z)
    complex(10), parameter :: p_cos = cos(z)
    complex(10), parameter :: p_tan = tan(z)
    complex(10), parameter :: p_sinh = sinh(z)
    complex(10), parameter :: p_cosh = cosh(z)
    complex(10), parameter :: p_tanh = tanh(z)
    complex(10), parameter :: p_asin = asin(h)
    complex(10), parameter :: p_acos = acos(h)
    complex(10), parameter :: p_atan = atan(h)
    complex(10), parameter :: p_asinh = asinh(h)
    complex(10), parameter :: p_acosh = acosh(h)
    complex(10), parameter :: p_atanh = atanh(h)
    complex(10), parameter :: p_conjg = conjg((1.0_10, 1.0_10))
    complex(10), parameter :: p_expr = 2 * exp(z) - conjg(z)
    real(10), parameter :: p_abs = abs((3.0_10, 4.0_10))
    real(10), parameter :: p_abs_z = abs(z)
    real(10), parameter :: p_real = real((3.0_10, 4.0_10))
    real(10), parameter :: p_aimag = aimag((3.0_10, 4.0_10))
    real(8), parameter :: p_real8 = real((3.0_10, 4.0_10), kind=8)
    complex(8), parameter :: p_c10_to_8 = cmplx((1.0_10, 2.0_10), kind=8)
    complex(10), parameter :: p_c8_to_10 = cmplx((1.0_8, 2.0_8), kind=10)
    complex(10), parameter :: p_c10_to_10 = cmplx(z, kind=10)
    complex(10), parameter :: p_nested = &
        cmplx(cmplx((1.0_10, 2.0_10), kind=8), kind=10)
    complex(10), parameter :: p_third = cmplx(1.0_10 / 3, 1, kind=10)
    complex(8), parameter :: p_third8 = cmplx(p_third, kind=8)
    complex(10), parameter :: p_merge_t = merge(z, h, .true.)
    complex(10), parameter :: p_merge_f = merge(z, h, .false.)
    complex(10), parameter :: p_merge_a(2) = merge(z, h, [.true., .false.])
    real(10), parameter :: tol = 8 * epsilon(1.0_10)
    complex(10) :: v, w

    ! Exact results
    if (p_conjg /= (1.0_10, -1.0_10)) error stop 1
    if (p_abs /= 5.0_10) error stop 2
    if (p_real /= 3.0_10) error stop 3
    if (p_aimag /= 4.0_10) error stop 4
    if (p_real8 /= 3.0_8) error stop 5
    if (p_c10_to_8 /= (1.0_8, 2.0_8)) error stop 6
    if (p_c8_to_10 /= (1.0_10, 2.0_10)) error stop 7
    if (p_c10_to_10 /= z) error stop 8
    if (p_nested /= (1.0_10, 2.0_10)) error stop 9
    if (p_sqrt34 /= (2.0_10, 1.0_10)) error stop 10
    if (p_third8 /= cmplx(1.0_8 / 3, 1, kind=8)) error stop 11
    if (p_merge_t /= z) error stop 16
    if (p_merge_f /= h) error stop 17
    if (p_merge_a(1) /= z) error stop 18
    if (p_merge_a(2) /= h) error stop 19

    ! Values to the precision of real(10)
    if (.not. near(p_exp, (1.46869393991588515717_10, &
        2.28735528717884239124_10))) error stop 12
    if (.not. near(p_sqrt, (1.09868411346780996607_10, &
        0.455089860562227341311_10))) error stop 13
    if (abs(p_abs_z - sqrt(2.0_10)) > tol) error stop 14
    if (abs(p_expr - (2 * p_exp - (1.0_10, -1.0_10))) > tol) error stop 15

    ! The folded values equal the values computed at run time
    v = z
    if (.not. near(p_exp, exp(v))) error stop 20
    if (.not. near(p_sqrt, sqrt(v))) error stop 21
    if (.not. near(p_log, log(v))) error stop 22
    if (.not. near(p_sin, sin(v))) error stop 23
    if (.not. near(p_cos, cos(v))) error stop 24
    if (.not. near(p_tan, tan(v))) error stop 25
    if (.not. near(p_sinh, sinh(v))) error stop 26
    if (.not. near(p_cosh, cosh(v))) error stop 27
    if (.not. near(p_tanh, tanh(v))) error stop 28
    w = h
    if (.not. near(p_asin, asin(w))) error stop 29
    if (.not. near(p_acos, acos(w))) error stop 30
    if (.not. near(p_atan, atan(w))) error stop 31
    if (.not. near(p_asinh, asinh(w))) error stop 32
    if (.not. near(p_acosh, acosh(w))) error stop 33
    if (.not. near(p_atanh, atanh(w))) error stop 34

    ! Inverse functions recover the argument
    if (.not. near(exp(p_log), z)) error stop 40
    if (.not. near(sin(p_asin), h)) error stop 41
    if (.not. near(cos(p_acos), h)) error stop 42
    if (.not. near(tan(p_atan), h)) error stop 43
    if (.not. near(p_sqrt * p_sqrt, z)) error stop 44

    print *, p_exp, p_sqrt, p_log
    print *, p_asin, p_acos, p_atan

contains

    logical function near(a, b)
        complex(10), intent(in) :: a, b
        near = abs(a - b) <= tol * abs(b)
    end function

end program
