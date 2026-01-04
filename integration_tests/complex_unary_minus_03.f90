! Test for complex unary minus in implicit interface call with PARAMETER
! Reproduces ICE from LAPACK clasyf_aa.f
subroutine csub(alpha, n, ok)
    implicit none
    integer, intent(in) :: n
    complex, intent(in) :: alpha
    logical, intent(out) :: ok
    ok = (real(alpha) == -1.0) .and. (aimag(alpha) == 0.0)
end subroutine

program complex_unary_minus_03
    implicit none
    complex, parameter :: zero = 0.0e+0, one = 1.0e+0
    logical :: ok
    external :: csub
    ok = .false.
    call csub(-one, 1, ok)
    if (.not. ok) error stop
    print *, 'PASS'
end program
