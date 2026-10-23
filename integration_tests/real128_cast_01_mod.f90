module real128_cast_01_mod
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
contains
    subroutine scale_half(incr, x)
        real(real128), intent(in) :: incr
        real(real128), intent(out) :: x
        ! 0.5 is default real. Folding the cast to real128 must pack a
        ! kind-16 constant, or writing this module file segfaults.
        x = 0.5 * incr
    end subroutine scale_half
end module real128_cast_01_mod
