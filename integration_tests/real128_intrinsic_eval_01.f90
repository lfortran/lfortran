program real128_intrinsic_eval_01
    implicit none
    integer, parameter :: qp = 16
    real(qp), parameter :: tiny_q = 1.0e-30_qp
    real(qp), parameter :: big = 1.0_qp + tiny_q
    real(qp), parameter :: below2 = 2.0_qp - tiny_q
    real(qp), parameter :: half_below = 0.5_qp - tiny_q
    real(qp), parameter :: tol = 1.0e-33_qp

    real(qp), parameter :: abs_r = abs(-big)
    real(qp), parameter :: sign_r = sign(big, -1.0_qp)
    real(qp), parameter :: dim_r = dim(big, 1.0_qp)
    real(qp), parameter :: dim_zero = dim(1.0_qp, big)
    real(qp), parameter :: max_r = max(big, 1.0_qp, -3.0_qp)
    real(qp), parameter :: min_r = min(big, 2.0_qp, 7.0_qp)
    real(qp), parameter :: aint_r = aint(below2)
    real(qp), parameter :: anint_r = anint(half_below)
    real(qp), parameter :: anint_up = anint(-1.5_qp)
    real(qp), parameter :: mod_r = mod(7.0_qp + tiny_q, 2.0_qp)
    real(qp), parameter :: modulo_r = modulo(-7.0_qp - tiny_q, 2.0_qp)
    real(qp), parameter :: scale_r = scale(big, 3)
    integer, parameter :: floor_r = floor(below2)
    integer, parameter :: ceiling_r = ceiling(big)
    integer, parameter :: nint_r = nint(half_below)
    integer, parameter :: nint_up = nint(0.5_qp)
    integer(8), parameter :: floor_big = floor(1.0e18_qp + 0.5_qp, 8)
    real(qp), parameter :: sind_r = sind(30.0_qp)
    real(qp), parameter :: cosd_r = cosd(60.0_qp)
    real(qp), parameter :: asind_r = asind(0.5_qp)
    real(qp), parameter :: sub = 3.0e-4940_qp
    real(qp), parameter :: mod_neg = mod(-7.5_qp, 2.0_qp)
    real(qp), parameter :: mod_negp = mod(7.5_qp, -2.0_qp)
    real(qp), parameter :: sub_fraction = fraction(sub)
    real(qp), parameter :: sub_set_exp = set_exponent(sub, 5)
    integer, parameter :: sub_exponent = exponent(sub)
    real(qp), parameter :: sub_spacing = spacing(sub)
    real(qp), parameter :: mod_long = mod(7.1_qp, 2.3_qp)
    real(qp), parameter :: mod_far = mod(1.0e40_qp, 3.0_qp)
    real(qp), parameter :: modulo_long = modulo(-7.1_qp, 2.3_qp)
    real(qp), parameter :: modulo_far = modulo(-1.0e40_qp, 3.0_qp)
    integer(8), parameter :: floor_edge = floor(9.0e18_qp, 8)
    integer(8), parameter :: ceiling_neg = ceiling(-9.0e18_qp - 0.5_qp, 8)
    integer(8), parameter :: nint_neg_half = nint(-2.5_qp, 8)
    real(qp), parameter :: sign_negzero = sign(2.0_qp, -0.0_qp)
    real(qp), parameter :: scale_far = scale(tiny(1.0_qp), 16400)
    complex(qp), parameter :: cmplx_r = cmplx(big, below2, qp)
    real(qp), parameter :: atand_r = atand(1.0_qp)
    real(qp), parameter :: eps_r = spacing(1.0_qp)
    real(qp), parameter :: spacing_3 = spacing(3.0_qp)
    real(qp), parameter :: spacing_zero = spacing(0.0_qp)
    real(qp), parameter :: next_up = nearest(1.0_qp, 1.0_qp)
    real(qp), parameter :: next_down = nearest(1.0_qp, -1.0_qp)
    real(qp), parameter :: next_neg = nearest(-1.0_qp, 1.0_qp)
    real(qp), parameter :: rr_r = rrspacing(3.0_qp)
    real(qp), parameter :: fraction_r = fraction(12.0_qp)
    real(qp), parameter :: set_exp_r = set_exponent(12.0_qp, 2)
    integer, parameter :: exponent_r = exponent(12.0_qp)
    integer, parameter :: exponent_zero = exponent(0.0_qp)

    if (abs_r /= big) error stop 1
    if (sign_r /= -big) error stop 2
    if (abs(dim_r - tiny_q) > tol) error stop 3
    if (dim_zero /= 0.0_qp) error stop 4
    if (max_r /= big) error stop 5
    if (min_r /= big) error stop 6
    if (aint_r /= 1.0_qp) error stop 7
    if (anint_r /= 0.0_qp) error stop 8
    if (anint_up /= -2.0_qp) error stop 9
    if (abs(mod_r - (1.0_qp + tiny_q)) > tol) error stop 10
    if (abs(modulo_r - (1.0_qp - tiny_q)) > tol) error stop 11
    if (scale_r /= 8.0_qp * big) error stop 12
    if (floor_r /= 1) error stop 13
    if (ceiling_r /= 2) error stop 14
    if (nint_r /= 0) error stop 15
    if (nint_up /= 1) error stop 16
    if (floor_big /= 1000000000000000000_8) error stop 17
    if (abs(sind_r - 0.5_qp) > 1.0e-30_qp) error stop 18
    if (abs(cosd_r - 0.5_qp) > 1.0e-30_qp) error stop 19
    if (abs(asind_r - 30.0_qp) > 1.0e-28_qp) error stop 20
    if (abs(atand_r - 45.0_qp) > 1.0e-28_qp) error stop 21
    if (eps_r /= epsilon(1.0_qp)) error stop 22
    if (spacing_3 /= 2.0_qp * epsilon(1.0_qp)) error stop 23
    if (spacing_zero /= tiny(1.0_qp)) error stop 24
    if (next_up /= 1.0_qp + epsilon(1.0_qp)) error stop 25
    if (next_down /= 1.0_qp - epsilon(1.0_qp) / 2.0_qp) error stop 26
    if (next_neg /= -(1.0_qp - epsilon(1.0_qp) / 2.0_qp)) error stop 27
    if (rr_r /= 0.75_qp * 2.0_qp ** 113) error stop 28
    if (fraction_r /= 0.75_qp) error stop 29
    if (set_exp_r /= 3.0_qp) error stop 30
    if (exponent_r /= 4) error stop 31
    if (exponent_zero /= 0) error stop 32
    if (mod_neg /= -1.5_qp) error stop 33
    if (mod_negp /= 1.5_qp) error stop 34
    if (abs(sub_fraction - 0.5988114683883382345471512836198786_qp) > 1.0e-30_qp) error stop 35
    if (abs(sub_set_exp - 19.16196698842682350550884107583611_qp) > 1.0e-28_qp) error stop 36
    if (sub_exponent /= -16408) error stop 37
    if (sub_spacing /= tiny(1.0_qp)) error stop 38
    if (abs(mod_long - 2.000000000000000000000000000000001541e-01_qp) > 1.0e-34_qp) error stop 39
    if (mod_far /= 1.0_qp) error stop 40
    if (abs(modulo_long - 2.099999999999999999999999999999999692_qp) > 1.0e-33_qp) error stop 41
    if (modulo_far /= 2.0_qp) error stop 42
    if (floor_edge /= 9000000000000000000_8) error stop 43
    if (ceiling_neg /= -9000000000000000000_8) error stop 44
    if (nint_neg_half /= -3_8) error stop 45
    if (sign_negzero /= -2.0_qp) error stop 46
    if (scale_far /= 262144.0_qp) error stop 47
    if (real(cmplx_r) /= big) error stop 48
    if (aimag(cmplx_r) /= below2) error stop 49
    print *, "ok"
end program
