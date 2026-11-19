program namelist_47
    implicit none
    real :: x, a(2)
    real(8) :: y
    complex :: z
    complex(8) :: w
    character(16) :: text
    logical :: flag
    character(256) :: s
    namelist /nl/ x, a, y, z, w, text, flag

    s = "&nl x=1d2, a=1D1,2q2, y=1.5Q+3, z=(1d1,2D1), w=(1q2,-2Q-1), text='dDqQ', flag=.true. /"
    read(s, nml=nl)
    if (x /= 100.0) error stop 1
    if (any(a /= [10.0, 200.0])) error stop 2
    if (y /= 1500.0_8) error stop 3
    if (z /= cmplx(10.0, 20.0)) error stop 4
    if (abs(w - cmplx(100.0_8, -0.2_8, kind=8)) > 1.e-14_8) error stop 5
    if (text /= 'dDqQ') error stop 6
    if (.not. flag) error stop 7

    s = "&nl x=1d-2, a=2*1D+2, y=1.d2, z=(-1Q-1,2q+1), w=(1D1,2d1) /"
    read(s, nml=nl)
    if (abs(x - 0.01) > 1.e-7) error stop 8
    if (any(a /= [100.0, 100.0])) error stop 9
    if (y /= 100.0_8) error stop 10
    if (abs(z - cmplx(-0.1, 20.0)) > 1.e-6) error stop 11
    if (w /= cmplx(10.0_8, 20.0_8, kind=8)) error stop 12

    s = "&nl x=1e2, y=2E-1, z=(1e1,2E1), w=(-1E2,2e1) /"
    read(s, nml=nl)
    if (x /= 100.0) error stop 13
    if (abs(y - 0.2_8) > 1.e-14_8) error stop 14
    if (z /= cmplx(10.0, 20.0)) error stop 15
    if (w /= cmplx(-100.0_8, 20.0_8, kind=8)) error stop 16
end program
