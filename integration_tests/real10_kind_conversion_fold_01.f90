program real10_kind_conversion_fold_01
    ! Kind conversions of real(10) constants are folded: exactly when widening
    ! to real(16), rounded to nearest when narrowing to real(10), real(8) or
    ! real(4), for array and scalar named constants and initializers
    implicit none
    real(10), parameter :: c(2) = [1.0_10, 2.0_10] / 3
    real(16), parameter :: q3(2) = [1.0_16, 2.0_16] / 3

    ! real(10) -> real(16)
    real(16), parameter :: q_c(2) = c
    real(16), parameter :: q_lit(2) = [1.0_10, 2.0_10]
    real(16), parameter :: q_real(2) = real(c, 16)
    real(16), parameter :: q_s = 1.0_10 / 3
    real(16) :: q_v(2) = [1.0_10, 2.0_10]
    real(16) :: q_vc(2) = c

    ! real(16) -> real(10)
    real(10), parameter :: x_q(2) = q3
    real(10), parameter :: x_lit(2) = [1.0_16, 2.0_16]
    real(10), parameter :: x_real(2) = real(q3, 10)
    real(10), parameter :: x_s = 1.0_16 / 3
    real(10) :: x_v(2) = [1.0_16, 2.0_16] / 3
    real(10) :: x_u(3) = [1.5_16, 2.0_16, 3.0_16]
    real(10) :: x_b(2) = 1.5_16
    real(16) :: q_b(2) = 1.5_10
    ! Halfway between two real(10) values: rounded to even
    real(10), parameter :: x_tie(2) = &
        [1.0_16 + 2.0_16**(-64), 1.0_16 + 3 * 2.0_16**(-64)]

    ! real(10) <-> real(8)
    real(8), parameter :: d_c(2) = c
    real(8), parameter :: d_lit(2) = [1.0_10, 2.0_10]
    real(8), parameter :: d_real(2) = real(c, 8)
    real(8) :: d_v(2) = [1.0_10, 2.0_10] / 3
    real(10), parameter :: x_d(2) = [1.0_8, 2.0_8] / 3
    real(10) :: x_dv(2) = [1.0_8, 2.0_8] / 3

    ! real(10) <-> real(4)
    real(4), parameter :: f_c(2) = c
    real(4) :: f_v(2) = [1.0_10, 2.0_10] / 3
    real(10), parameter :: x_f(2) = [1.0_4, 2.0_4] / 3
    real(10) :: x_fv(2) = [1.0_4, 2.0_4] / 3

    ! real(10) -> complex(16)
    complex(16), parameter :: z_c(2) = c
    complex(16), parameter :: z_cm(2) = cmplx(c, q3, 16)
    complex(16), parameter :: z_s = cmplx(1.0_10 / 3, 2.0_10, 16)

    real(10) :: c_rt(2), x_rt(2)
    real(16) :: q_rt(2)
    real(8) :: d_rt(2)
    real(4) :: f_rt(2)

    ! Run-time conversions of the same values
    c_rt = c
    q_rt = q3
    d_rt = [1.0_8, 2.0_8] / 3
    f_rt = [1.0_4, 2.0_4] / 3

    if (any(q_c /= real(c_rt, 16))) error stop "q_c"
    if (q_c(1) /= 0.333333333333333333342368351437379204_16) error stop "q_c 1"
    if (any(q_lit /= [1.0_16, 2.0_16])) error stop "q_lit"
    if (any(q_real /= q_c)) error stop "q_real"
    if (q_s /= q_c(1)) error stop "q_s"
    if (any(q_v /= [1.0_16, 2.0_16])) error stop "q_v"
    if (any(q_vc /= q_c)) error stop "q_vc"

    x_rt = real(q_rt, 10)
    if (any(x_q /= x_rt)) error stop "x_q"
    if (any(x_q /= c)) error stop "x_q c"
    if (x_q(1) /= 0.333333333333333333342_10) error stop "x_q 1"
    if (any(x_lit /= [1.0_10, 2.0_10])) error stop "x_lit"
    if (any(x_real /= x_q)) error stop "x_real"
    if (x_s /= x_q(1)) error stop "x_s"
    if (any(x_v /= x_q)) error stop "x_v"
    if (any(x_u /= [1.5_10, 2.0_10, 3.0_10])) error stop "x_u"
    if (any(x_b /= 1.5_10)) error stop "x_b"
    if (any(q_b /= 1.5_16)) error stop "q_b"
    if (any(real(z_c, 16) /= q_c) .or. any(aimag(z_c) /= 0)) error stop "z_c"
    if (any(real(z_cm, 16) /= q_c) .or. any(aimag(z_cm) /= q3)) error stop "z_cm"
    if (real(z_s, 16) /= q_c(1) .or. aimag(z_s) /= 2) error stop "z_s"
    if (x_tie(1) /= 1.0_10) error stop "x_tie 1"
    if (x_tie(2) /= 1.0_10 + 2.0_10**(-62)) error stop "x_tie 2"

    if (any(d_c /= [1.0_8, 2.0_8] / 3)) error stop "d_c"
    if (any(d_c /= real(c_rt, 8))) error stop "d_c rt"
    if (any(d_lit /= [1.0_8, 2.0_8])) error stop "d_lit"
    if (any(d_real /= d_c)) error stop "d_real"
    if (any(d_v /= d_c)) error stop "d_v"
    if (any(x_d /= real(d_rt, 10))) error stop "x_d"
    if (any(x_dv /= x_d)) error stop "x_dv"

    if (any(f_c /= real(c_rt, 4))) error stop "f_c"
    if (any(f_c /= [1.0_4, 2.0_4] / 3)) error stop "f_c 4"
    if (any(f_v /= f_c)) error stop "f_v"
    if (any(x_f /= real(f_rt, 10))) error stop "x_f"
    if (any(x_fv /= x_f)) error stop "x_fv"
    print *, "ok"
end program
