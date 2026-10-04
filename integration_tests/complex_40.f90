program complex_40
! complex**integer is evaluated by repeated multiplication, so powers of
! Gaussian integers are exact (https://github.com/lfortran/lfortran/issues/3823)
implicit none
integer, parameter :: sp = kind(0.0), dp = kind(0.d0)
complex(dp), parameter :: zp2 = (0.0_dp, 1.0_dp)**2
complex(dp), parameter :: zpm3 = (0.0_dp, 1.0_dp)**(-3)
complex(sp), parameter :: zsp3 = (0.0_sp, 1.0_sp)**3
complex(dp) :: z, w, c, zarr(3), expected(0:3)
complex(sp) :: zs, ws
real(dp) :: x
integer :: n, k, ix, n_inside
integer(8) :: n8
logical :: z_inside, x_inside

expected = [(1.0_dp, 0.0_dp), (0.0_dp, 1.0_dp), (-1.0_dp, 0.0_dp), &
    (0.0_dp, -1.0_dp)]

! Constant exponents
z = (0.0_dp, 1.0_dp)
if (z**2 /= (-1.0_dp, 0.0_dp)) error stop
if (z**3 /= (0.0_dp, -1.0_dp)) error stop
if (z**4 /= (1.0_dp, 0.0_dp)) error stop
if (z**0 /= (1.0_dp, 0.0_dp)) error stop
if (z**1 /= z) error stop
if (z**(-1) /= (0.0_dp, -1.0_dp)) error stop
if (z**(-2) /= (-1.0_dp, 0.0_dp)) error stop
if (z**13 /= (0.0_dp, 1.0_dp)) error stop

zs = (0.0_sp, 1.0_sp)
if (zs**2 /= (-1.0_sp, 0.0_sp)) error stop
if (zs**3 /= (0.0_sp, -1.0_sp)) error stop
if (zs**4 /= (1.0_sp, 0.0_sp)) error stop
if (zs**(-1) /= (0.0_sp, -1.0_sp)) error stop

! Compile-time constants
if (zp2 /= (-1.0_dp, 0.0_dp)) error stop
if (zpm3 /= (0.0_dp, 1.0_dp)) error stop
if (zsp3 /= (0.0_sp, -1.0_sp)) error stop
if ((1.0_dp, 1.0_dp)**8 /= (16.0_dp, 0.0_dp)) error stop

! Variable exponents
do n = -9, 9
    w = z**n
    if (w /= expected(modulo(n, 4))) error stop
    ws = zs**n
    if (ws /= cmplx(expected(modulo(n, 4)), kind=sp)) error stop
end do
w = (1.0_dp, 1.0_dp)
n = 8
if (w**n /= (16.0_dp, 0.0_dp)) error stop
n = 10
if (w**n /= (0.0_dp, 32.0_dp)) error stop
n = -2
if (w**n /= (0.0_dp, -0.5_dp)) error stop
n8 = 3
if (w**n8 /= (-2.0_dp, 2.0_dp)) error stop
ws = (1.0_sp, -1.0_sp)
if (ws**n8 /= (-2.0_sp, -2.0_sp)) error stop

! A general base agrees with explicit multiplication
w = (1.1_dp, 0.7_dp)
n = 5
if (abs(w**n - w*w*w*w*w) > 1e-12_dp) error stop
if (abs(w**5 - w*w*w*w*w) > 1e-12_dp) error stop
if (abs(w**(-n) - 1/(w*w*w*w*w)) > 1e-12_dp) error stop

! Arrays
zarr = [(0.0_dp, 1.0_dp), (1.0_dp, 1.0_dp), (2.0_dp, 0.0_dp)]
zarr = zarr**2
if (any(zarr /= [(-1.0_dp, 0.0_dp), (0.0_dp, 2.0_dp), (4.0_dp, 0.0_dp)])) &
    error stop
zarr = [(0.0_dp, 1.0_dp), (1.0_dp, 1.0_dp), (2.0_dp, 0.0_dp)]
zarr = zarr**[3, 2, -1]
if (any(zarr /= [(0.0_dp, -1.0_dp), (0.0_dp, 2.0_dp), (0.5_dp, 0.0_dp)])) &
    error stop

! Mandelbrot iteration on the real axis stays real and agrees with the real
! iteration on which points escape
n_inside = 0
do ix = -40, 10
    c = cmplx(ix / 20.0_dp, 0.0_dp, kind=dp)
    z = (0.0_dp, 0.0_dp)
    x = 0.0_dp
    z_inside = .true.
    x_inside = .true.
    do k = 1, 50
        z = z**2 + c
        x = x**2 + real(c, dp)
        if (aimag(z) /= 0.0_dp) error stop
        if (abs(z) > 2.0_dp) z_inside = .false.
        if (abs(x) > 2.0_dp) x_inside = .false.
        if (.not. z_inside) exit
    end do
    if (z_inside .neqv. x_inside) error stop
    if (z_inside) n_inside = n_inside + 1
end do
print *, n_inside
if (n_inside /= 46) error stop
end program
