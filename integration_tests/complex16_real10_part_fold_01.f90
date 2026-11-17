program complex16_real10_part_fold_01
    ! The real part of a complex(16) constant converted to real(10) is
    ! rounded once, not through a double
    implicit none
    real(16), parameter :: a = 1.0_16 / 3
    real(16), parameter :: b = 2.0_16 / 7
    complex(16), parameter :: c = cmplx(a, b, 16)
    complex(16), parameter :: ca(2) = [c, conjg(c)]
    real(10), parameter :: ra = real(a, 10)
    real(10), parameter :: rb = real(b, 10)
    real(10), parameter :: x1 = real(c, 10)
    real(10), parameter :: x2 = real(cmplx(a, 0, 16), 10)
    real(10), parameter :: x3 = c
    real(10), parameter :: x4(2) = real(ca, 10)
    real(10), parameter :: r(2) = [ra, rb]
    real(10) :: y(2), z
    y = real(cmplx(r, r, 16), 10)
    z = real(c, 10)
    print *, x1, x2, x3, x4, y, z
    if (x1 /= ra) error stop 1
    if (x2 /= ra) error stop 2
    if (x3 /= ra) error stop 3
    if (any(x4 /= ra)) error stop 4
    if (y(1) /= ra .or. y(2) /= rb) error stop 5
    if (z /= ra) error stop 6
end program
