module lean_sin_kernels_01_mod
use, intrinsic :: iso_fortran_env, only: dp => real64
implicit none
real(dp), parameter :: pi = 3.141592653589793_dp

contains

! Accurate on [-pi/2,pi/2] to about 1e-15 in relative accuracy
elemental real(dp) function kernel_sin(x) result(res)
real(dp), intent(in) :: x
real(dp), parameter :: S1 = 1
real(dp), parameter :: S2 = -0.16666666666665748417_dp
real(dp), parameter :: S3 = 8.333333333260810195e-3_dp
real(dp), parameter :: S4 = -1.9841269819408224684e-4_dp
real(dp), parameter :: S5 = 2.7557315969010714494e-6_dp
real(dp), parameter :: S6 = -2.5051843446312301534e-8_dp
real(dp), parameter :: S7 = 1.6047020166520616231e-10_dp
real(dp), parameter :: S8 = -7.360938387054769116e-13_dp
real(dp) :: z
z = x*x
res = x * (S1+z*(S2+z*(S3+z*(S4+z*(S5+z*(S6+z*(S7+z*S8)))))))
end function

! Accurate to about 1e-15 for |x0| < pi*2^31: three-part (Cody-Waite)
! reduction by pi, then kernel_sin
elemental real(dp) function kernel_fast(x0) result(res)
real(dp), intent(in) :: x0
real(dp), parameter :: p1 = 3.14159202575683594e+00_dp
real(dp), parameter :: p2 = 6.27832832833519205e-07_dp
real(dp), parameter :: p3 = 1.24467443437932268e-13_dp
real(dp) :: x, Nd
x = x0
Nd = int(x/pi + 0.5_dp*sign(1._dp, x))
x = ((x - Nd*p1) - Nd*p2) - Nd*p3
! -pi/2 < x < pi/2
! For even Nd, we have sin(x0) = sin(x)
! For odd Nd,  we have sin(x0) = sin(x+pi) = -sin(x) = sin(-x)
if (modulo(int(Nd), 2) == 1) x = -x
res = kernel_sin(x)
end function

end module lean_sin_kernels_01_mod

program lean_sin_kernels_01
use lean_sin_kernels_01_mod
implicit none
real(dp) :: x
integer :: i
do i = -20, 20
    x = 0.37_dp * i
    if (abs(kernel_fast(x) - sin(x)) > 3e-15_dp) error stop
    x = 123456.789_dp * i + 0.5_dp
    if (abs(kernel_fast(x) - sin(x)) > 3e-15_dp) error stop
    x = 1e8_dp * i + 0.25_dp
    if (abs(kernel_fast(x) - sin(x)) > 3e-15_dp) error stop
end do
x = 0.5_dp
if (abs(kernel_sin(x) - sin(x)) > 1e-15_dp) error stop
x = 5.9e9_dp
if (abs(kernel_fast(x) - sin(x)) > 3e-15_dp) error stop
print *, kernel_sin(0.5_dp), kernel_fast(1e9_dp)
end program
