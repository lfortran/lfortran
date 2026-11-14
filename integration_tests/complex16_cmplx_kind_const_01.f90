program complex16_cmplx_kind_const_01
    complex(16), parameter :: z = cmplx(1.5_8, 2.5_8, kind=16)
    real(16), parameter :: x = real(2.5_8, 16)
    real(16), parameter :: q = 3.5_16
    real(8), parameter :: d = real(q, 8)
    real(4), parameter :: s = real(q, 4)
    real(16), parameter :: w = real(0.1_8, 16)
    real(8), parameter :: h = real(1.1_16, 8)
    if (w /= real(0.1_8, 16) .or. w == 0.1_16) error stop 5
    if (h /= 1.1_8) error stop 6
    if (z /= (1.5_16, 2.5_16)) error stop 1
    if (x /= 2.5_16) error stop 2
    if (d /= 3.5_8) error stop 3
    if (s /= 3.5_4) error stop 4
end program
