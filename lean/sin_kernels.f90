module sin_kernels
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

! Accurate to about 6e-3 in absolute accuracy, for any x
elemental real(dp) function kernel_fastest(x0) result(res)
real(dp), intent(in) :: x0
real(dp), parameter :: S1 =  0.982396485658623
real(dp), parameter :: S2 = -0.14013802346642243
real(dp) :: x, z, Nd
x = x0
Nd = int(x/pi + 0.5_dp*sign(1._dp, x))
x = x - Nd*pi
! -pi/2 < x < pi/2
! For even Nd, we have sin(x0) = sin(x)
! For odd Nd,  we have sin(x0) = sin(x+pi) = -sin(x) = sin(-x)
if (modulo(int(Nd), 2) == 1) x = -x
z = x*x
res = x*(S1+z*S2)
end function

end module
