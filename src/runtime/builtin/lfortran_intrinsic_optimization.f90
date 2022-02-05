module lfortran_intrinsic_optimization
use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64, real128
implicit none

interface flipsign
    module procedure flipsigni32r32, flipsigni32r64
end interface

interface fma
    module procedure fmar32, fmar64
end interface

contains

! ------- flipsign procedures

subroutine flipsigni32r32(signal, variable)
integer(int32), intent(in) :: signal
real(real32), intent(out) :: variable
integer(int32) :: q
q = signal/2
if (signal - 2*q == 1 ) variable = -variable
end subroutine

subroutine flipsigni32r64(signal, variable)
integer(int32), intent(in) :: signal
real(real64), intent(out) :: variable
integer(int64) :: q
q = signal/2
if (signal - 2*q == 1 ) variable = -variable
end subroutine

! ------- fma procedures

elemental real(real32) function fmar32(a, b, c) result(d)
    real(real32), intent(in) :: a, b, c
    d = a + b * c
end function

elemental real(real64) function fmar64(a, b, c) result(d)
    real(real64), intent(in) :: a, b, c
    d = a + b * c
end function

end module
