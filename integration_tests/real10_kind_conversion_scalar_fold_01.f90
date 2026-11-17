program real10_kind_conversion_scalar_fold_01
    ! Kind conversions of real(10) scalar constants are folded: exactly when
    ! widening to real(16), rounded once to nearest when narrowing to
    ! real(10), real(8) or real(4), for named constants and initializers
    implicit none
    ! real(10) -> real(16)
    real(16), parameter :: q_s = 1.0_10 / 3
    real(16), parameter :: q_real = real(1.0_10 / 3, 16)
    real(16) :: q_v = 1.0_10 / 3
    real(16), parameter :: q_eps = 1.0_10 + 2.0_10**(-63)

    ! real(16) -> real(10)
    real(10), parameter :: x_s = 1.0_16 / 3
    real(10), parameter :: x_real = real(1.0_16 / 3, 10)
    real(10) :: x_v = 1.0_16 / 3
    ! Halfway between two real(10) values: rounded to even
    real(10), parameter :: x_tie1 = 1.0_16 + 2.0_16**(-64)
    real(10), parameter :: x_tie2 = 1.0_16 + 3 * 2.0_16**(-64)

    ! real(10) <-> real(8)
    real(8), parameter :: d_s = 1.0_10 / 3
    real(8), parameter :: d_real = real(1.0_10 / 3, 8)
    real(10), parameter :: x_d = 1.0_8 / 3

    ! real(10) -> real(4): 1 + 2**-24 + 2**-62 rounds to 1 + 2**-23 in
    ! real(4), but to 1 + 2**-24 in double and then to 1
    real(4), parameter :: f_s = 1.0_10 + 2.0_10**(-24) + 2.0_10**(-62)
    real(4), parameter :: f_real = real(1.0_10 + 2.0_10**(-24) + 2.0_10**(-62), 4)
    real(4) :: f_v = 1.0_10 + 2.0_10**(-24) + 2.0_10**(-62)

    ! real(10) -> complex(16)
    complex(16), parameter :: z_s = cmplx(1.0_10 / 3, 2.0_10, 16)
    complex(16), parameter :: z_c = 1.0_10 / 3

    real(10) :: c_rt
    real(16) :: q_rt

    ! Run-time conversions of the same values
    c_rt = 1.0_10 / 3
    q_rt = 1.0_16 / 3

    if (q_s /= real(c_rt, 16)) error stop "q_s"
    if (q_s /= 0.333333333333333333342368351437379204_16) error stop "q_s value"
    if (q_real /= q_s .or. q_v /= q_s) error stop "q_real"
    if (q_eps /= 1.0_16 + 2.0_16**(-63)) error stop "q_eps"

    if (x_s /= real(q_rt, 10)) error stop "x_s"
    if (x_s /= c_rt) error stop "x_s c"
    if (x_real /= x_s .or. x_v /= x_s) error stop "x_real"
    if (x_tie1 /= 1.0_10) error stop "x_tie1"
    if (x_tie2 /= 1.0_10 + 2.0_10**(-62)) error stop "x_tie2"

    if (d_s /= 1.0_8 / 3 .or. d_s /= real(c_rt, 8)) error stop "d_s"
    if (d_real /= d_s) error stop "d_real"
    if (x_d /= real(1.0_8 / 3, 10)) error stop "x_d"

    if (f_s /= 1.0_4 + 2.0_4**(-23)) error stop "f_s"
    if (f_real /= f_s .or. f_v /= f_s) error stop "f_real"

    if (real(z_s, 16) /= q_s .or. aimag(z_s) /= 2) error stop "z_s"
    if (real(z_c, 16) /= q_s .or. aimag(z_c) /= 0) error stop "z_c"
    print *, "ok"
end program
