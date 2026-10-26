module real128_cast_01_mod
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128
    implicit none
    ! Default and double real constants converted to real128 are folded into
    ! kind-16 constants, which are written to this module file.
    real(real128), parameter :: half = 0.5
    real(real128), parameter :: quarter = 0.25_real64
    real(real128), parameter :: one = 0.5 * 2.0_real128
    ! real128 constants converted to a lower kind are folded as well.
    real(real64), parameter :: three_halves = 1.5_real128
    real(real32), parameter :: nine_quarters = 2.25_real128
contains
    subroutine scale_half(incr, x)
        real(real128), intent(in) :: incr
        real(real128), intent(out) :: x
        x = 0.5 * incr
    end subroutine scale_half
end module real128_cast_01_mod
