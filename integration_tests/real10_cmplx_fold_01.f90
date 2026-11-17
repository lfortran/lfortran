program real10_cmplx_fold_01
    ! cmplx of real(10) constants folds their values, for scalars and arrays
    implicit none
    real(10), parameter :: c(3) = [1.5_10, 2.0_10, 4.0_10] / 3
    real(10), parameter :: d(3) = [0.25_10, -0.5_10, 8.0_10]
    real(10), parameter :: s = 1.5_10
    complex(8), parameter :: z8(3) = cmplx(c, kind=8)
    complex(8), parameter :: zd8(3) = cmplx(c, d, 8)
    complex(4), parameter :: z4(3) = cmplx(c, kind=4)
    complex(8), parameter :: zs8 = cmplx(s, kind=8)
    complex(4), parameter :: zs4 = cmplx(s, 2.5_10, kind=4)
    real(10) :: c_rt(3), d_rt(3)
    complex(8) :: y8(3), ys8
    complex(4) :: y4(3)

    c_rt = c
    d_rt = d
    y8 = cmplx(c, d, 8)
    y4 = cmplx(c, kind=4)
    ys8 = cmplx(s, kind=8)

    if (any(z8 /= cmplx(real(c_rt, 8), 0.0_8, 8))) error stop "z8"
    if (any(zd8 /= cmplx(real(c_rt, 8), real(d_rt, 8), 8))) error stop "zd8"
    if (any(z4 /= cmplx(real(c_rt, 4), 0.0_4, 4))) error stop "z4"
    if (zs8 /= (1.5_8, 0.0_8)) error stop "zs8"
    if (zs4 /= (1.5_4, 2.5_4)) error stop "zs4"
    if (any(y8 /= zd8)) error stop "y8"
    if (any(y4 /= z4)) error stop "y4"
    if (ys8 /= zs8) error stop "ys8"
    print *, "ok"
end program
