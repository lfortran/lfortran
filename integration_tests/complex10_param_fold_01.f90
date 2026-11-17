program complex10_param_fold_01
    ! Arithmetic on complex(10) constants is folded in constant expressions
    implicit none
    complex(10), parameter :: a = (1.0_10, 2.0_10)
    complex(10), parameter :: p_mul = (1.0_10, 2.0_10) * (1.0_10, 2.0_10)
    complex(10), parameter :: p_add = (1.0_10, 2.0_10) + (3.0_10, 4.0_10)
    complex(10), parameter :: p_sub = a - (0.5_10, 0.25_10)
    complex(10), parameter :: p_neg = -(1.0_10, 2.0_10)
    complex(10), parameter :: p_neg_name = -a
    complex(10), parameter :: p_div = (1.0_10, 2.0_10) / (3.0_10, 4.0_10)
    complex(10), parameter :: p_third = (1.0_10, 0.0_10) / (3.0_10, 0.0_10)
    complex(10), parameter :: p_pow2 = a ** 2
    complex(10), parameter :: p_pow_neg = a ** (-2)
    complex(10), parameter :: p_pow_half = a ** 0.5_10
    complex(10), parameter :: p_int = a * 2
    complex(10), parameter :: p_real = a * 2.0_10
    complex(10), parameter :: p_c8 = a * (1.0_8, 2.0_8)
    complex(10), parameter :: p_cmplx = cmplx(1, 2, 10) + 1
    complex(10), parameter :: p_nested = (a + 1) * (a - 1) / 2
    complex(10), parameter :: p_arr(3) = [a, 2 * a, -a * 2]
    logical, parameter :: l_eq = a == (1.0_10, 2.0_10)
    logical, parameter :: l_ne = a /= (1.0_10, 2.0_10)
    logical, parameter :: l_eq_c8 = p_c8 == (-3.0_8, 4.0_8)
    complex(10) :: z, w
    real(10) :: r

    if (p_mul /= (-3.0_10, 4.0_10)) error stop 1
    if (p_add /= (4.0_10, 6.0_10)) error stop 2
    if (p_sub /= (0.5_10, 1.75_10)) error stop 3
    if (p_neg /= (-1.0_10, -2.0_10)) error stop 4
    if (p_neg_name /= (-1.0_10, -2.0_10)) error stop 5
    if (p_div /= (0.44_10, 0.08_10)) error stop 6
    if (p_pow2 /= (-3.0_10, 4.0_10)) error stop 7
    if (p_pow_neg /= (-0.12_10, -0.16_10)) error stop 8
    if (p_int /= (2.0_10, 4.0_10)) error stop 9
    if (p_real /= (2.0_10, 4.0_10)) error stop 10
    if (p_c8 /= (-3.0_10, 4.0_10)) error stop 11
    if (p_cmplx /= (2.0_10, 2.0_10)) error stop 12
    if (p_nested /= (-2.0_10, 2.0_10)) error stop 13
    if (.not. l_eq) error stop 14
    if (l_ne) error stop 15
    if (.not. l_eq_c8) error stop 16
    if (p_arr(1) /= a .or. p_arr(2) /= (2.0_10, 4.0_10)) error stop 23
    if (p_arr(3) /= (-2.0_10, -4.0_10)) error stop 24

    ! The folded values keep the precision of real(10)
    r = 1.0_10
    r = r / 3
    z = p_third
    if (real(z, 10) /= r .or. aimag(z) /= 0.0_10) error stop 17
    if (real(z, 8) == real(z, 10)) error stop 18

    ! The same operations at run time
    w = a
    z = w * w
    if (z /= p_mul) error stop 19
    z = w / (3.0_10, 4.0_10)
    if (z /= p_div) error stop 20
    z = w ** 0.5_10
    if (abs(z - p_pow_half) > 1e-17_10) error stop 21
    if (abs(p_pow_half ** 2 - a) > 1e-17_10) error stop 22

    print *, p_mul, p_div, p_pow_half
end program
