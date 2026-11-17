program real10_int_modulo_cmplx_fold_01
    ! int, modulo and cmplx(x, kind=4) of real(10) constants are folded
    ! exactly, not through double, for array and scalar named constants,
    ! initializers and expressions
    implicit none
    ! 2 - 2**-62 and 2**62 + 3.5 are not representable in double
    real(10), parameter :: c(3) = [2.0_10 - 2.0_10**(-62), 2.0_10**62 + 3.5_10, &
        -7.75_10]
    integer(8), parameter :: ei(3) = [1_8, 2_8**62 + 3, -7_8]
    integer(8), parameter :: pi8(3) = int(c, 8), si8 = int(c(2), 8)
    integer, parameter :: pi4(2) = int(c(1:3:2)), si4 = int(c(1))
    integer(2), parameter :: pi2(2) = int(c(1:3:2), 2)
    integer(8) :: vi8(3) = int(c, 8)
    integer(8) :: i8(3)
    integer :: i4(2)
    integer(2) :: i2(2)

    ! modulo: 3**30 is large compared with 0.7, so `a - b*floor(a/b)` cancels
    real(10), parameter :: m(3) = [3.0_10**30, -7.75_10, 2.0_10 - 2.0_10**(-62)]
    real(10), parameter :: pm(3) = modulo(m, 0.7_10), pmn(3) = modulo(m, -0.7_10)
    real(10), parameter :: pmr(2) = modulo(-1.0e25_10, m(1:2))
    real(10), parameter :: sm = modulo(m(1), 0.7_10)
    real(10) :: vm(3) = modulo(m, 0.7_10)
    real(10) :: mv(3), em(3), emn(3), emr(2), r(3)

    ! cmplx(x, kind=4): 1 + 2**-24 + 2**-62 rounds to 1 + 2**-23 in real(4),
    ! but to 1 + 2**-24 in double and then to 1
    real(10), parameter :: z(2) = [1.0_10 + 2.0_10**(-24) + 2.0_10**(-62), &
        -1.0_10 - 2.0_10**(-24) - 2.0_10**(-62)]
    real(4), parameter :: ez(2) = [1.0_4 + 2.0_4**(-23), -1.0_4 - 2.0_4**(-23)]
    complex(4), parameter :: pz(2) = cmplx(z, kind=4), pz2(2) = cmplx(z, z, kind=4)
    complex(4), parameter :: sz = cmplx(z(1), kind=4)
    complex(4) :: vz(2) = cmplx(z, kind=4)
    complex(4) :: cz(2)

    if (any(pi8 /= ei) .or. si8 /= ei(2) .or. any(vi8 /= ei)) error stop
    if (any(pi4 /= ei(1:3:2)) .or. si4 /= 1) error stop
    if (any(pi2 /= ei(1:3:2))) error stop
    i8 = int(c, 8)
    if (any(i8 /= ei)) error stop
    i4 = int(c(1:3:2))
    if (any(i4 /= ei(1:3:2))) error stop
    i2 = int(c(1:3:2), 2)
    if (any(i2 /= ei(1:3:2))) error stop

    ! The run time computes modulo exactly
    mv = m
    em = modulo(mv, 0.7_10)
    emn = modulo(mv, -0.7_10)
    emr = modulo(-1.0e25_10, mv(1:2))
    if (any(pm /= em) .or. any(pmn /= emn) .or. any(pmr /= emr)) error stop
    if (sm /= em(1) .or. any(vm /= em)) error stop
    r = modulo(m, 0.7_10)
    if (any(r /= em)) error stop
    r = modulo(m, -0.7_10)
    if (any(r /= emn)) error stop
    r(1:2) = modulo(-1.0e25_10, m(1:2))
    if (any(r(1:2) /= emr)) error stop

    if (any(real(pz) /= ez) .or. any(aimag(pz) /= 0)) error stop
    if (any(real(pz2) /= ez) .or. any(aimag(pz2) /= ez)) error stop
    if (real(sz) /= ez(1) .or. any(real(vz) /= ez)) error stop
    cz = cmplx(z, kind=4)
    if (any(real(cz) /= ez) .or. any(aimag(cz) /= 0)) error stop
    cz = cmplx(z, z, kind=4)
    if (any(real(cz) /= ez) .or. any(aimag(cz) /= ez)) error stop
    print *, i8, em, real(cz)
end program
