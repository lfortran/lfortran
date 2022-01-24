module lfortran_intrinsic_optimisation
use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64, real128
implicit none

interface flipsign
    module procedure flipsigni32r32, flipsigni32r64
end interface

contains

! ------- flipsign procedures

subroutine flipsigni32r32(signal, variable)
integer(int32), intent(in) :: signal
real(real32), intent(out) :: variable
end subroutine

subroutine flipsigni32r64(signal, variable)
integer(int32), intent(in) :: signal
real(real64), intent(out) :: variable
end subroutine

end module
