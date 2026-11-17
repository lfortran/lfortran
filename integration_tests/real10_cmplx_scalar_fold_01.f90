program real10_cmplx_scalar_fold_01
    ! cmplx of real(10) scalar constants folds their values, rounded once to
    ! the kind of the result
    implicit none
    real(10), parameter :: s = 1.5_10, t = 2.0_10 / 3
    ! 1 + 2**-24 + 2**-62 rounds to 1 + 2**-23 in real(4), but to 1 + 2**-24
    ! in double and then to 1
    real(10), parameter :: z = 1.0_10 + 2.0_10**(-24) + 2.0_10**(-62)
    real(4), parameter :: ez = 1.0_4 + 2.0_4**(-23)
    complex(8), parameter :: zs8 = cmplx(s, kind=8)
    complex(8), parameter :: zt8 = cmplx(t, s, 8)
    complex(4), parameter :: zs4 = cmplx(s, 2.5_10, kind=4)
    complex(4), parameter :: pz = cmplx(z, kind=4), pz2 = cmplx(z, -z, kind=4)
    complex(16), parameter :: zq = cmplx(t, kind=16)
    complex(8) :: vs8 = cmplx(1.5_10, kind=8)
    complex(4) :: vz = cmplx(z, kind=4)
    real(10) :: t_rt

    t_rt = t
    if (zs8 /= (1.5_8, 0.0_8)) error stop "zs8"
    if (zt8 /= cmplx(real(t_rt, 8), 1.5_8, 8)) error stop "zt8"
    if (zs4 /= (1.5_4, 2.5_4)) error stop "zs4"
    if (real(pz) /= ez .or. aimag(pz) /= 0) error stop "pz"
    if (real(pz2) /= ez .or. aimag(pz2) /= -ez) error stop "pz2"
    if (real(zq, 16) /= real(t_rt, 16) .or. aimag(zq) /= 0) error stop "zq"
    if (vs8 /= zs8) error stop "vs8"
    if (vz /= pz) error stop "vz"
    print *, zs8, pz
end program
