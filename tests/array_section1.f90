subroutine vecfcn(n)
    use iso_fortran_env, only: wp => real64
    implicit none
    integer :: n
    real(wp) :: fvec(n)
    real(wp) :: zero = 0.0_wp
    integer :: i

    fvec(2:n) = zero

end subroutine vecfcn
