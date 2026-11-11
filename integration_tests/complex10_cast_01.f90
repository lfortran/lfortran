program complex10_cast_01
    ! Conversions between real and complex of different kinds, with kind=10
    implicit none
    complex(10) :: z
    complex(8) :: z8
    complex(16) :: z16
    real(16) :: r16
    real(10) :: r10
    real(8) :: r8
    real(10) :: ra(2)
    complex(8) :: za(2)

    r16 = 1.5_16
    r10 = 2.5_10
    r8 = 3.5_8

    z = r16
    if (real(z, 10) /= 1.5_10 .or. aimag(z) /= 0.0_10) error stop 1
    z = cmplx(r16, kind=10)
    if (real(z, 10) /= 1.5_10 .or. aimag(z) /= 0.0_10) error stop 2
    z = cmplx(r16, r10, kind=10)
    if (real(z, 10) /= 1.5_10 .or. aimag(z) /= 2.5_10) error stop 3
    z = cmplx(r10, r16)
    if (real(z, 10) /= 2.5_10 .or. aimag(z) /= 1.5_10) error stop 4
    z = r8
    if (real(z, 10) /= 3.5_10 .or. aimag(z) /= 0.0_10) error stop 5

    z8 = r10
    if (real(z8, 8) /= 2.5_8 .or. aimag(z8) /= 0.0_8) error stop 6
    z8 = cmplx(r10, r10, kind=8)
    if (real(z8, 8) /= 2.5_8 .or. aimag(z8) /= 2.5_8) error stop 7
    z8 = cmplx(r10, kind=8)
    if (real(z8, 8) /= 2.5_8) error stop 8

    z16 = r10
    if (real(z16, 16) /= 2.5_16 .or. aimag(z16) /= 0.0_16) error stop 9
    z16 = cmplx(r10, r10, kind=16)
    if (real(z16, 16) /= 2.5_16 .or. aimag(z16) /= 2.5_16) error stop 10

    ra = [1.25_10, 2.25_10]
    za = ra
    if (real(za(2), 8) /= 2.25_8) error stop 11

    z = z + r16
    if (real(z, 10) /= 5.0_10) error stop 12
    z8 = z8 + r10
    if (real(z8, 8) /= 5.0_8) error stop 13
    print *, z, z8
end program
