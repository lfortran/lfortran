! Regression test: a bare `external` procedure declared inside a FUNCTION
! program unit must not leak into a sibling program unit processed afterwards.
! The external-procedure bookkeeping done while visiting a FUNCTION was never
! saved/restored (unlike a SUBROUTINE), so the name `get_msg` stayed marked as
! external when the following `check` subroutine was analysed. That dropped its
! explicit `character(len=80)` declaration, wrongly typing `get_msg` as `real`
! and producing a type mismatch / invalid code.
! Reduced from netcdf-fortran (nf_test/test_put.F).

double precision function hash_double()
    implicit none
    external get_msg
    hash_double = 0
end function

subroutine check(err, out)
    implicit none
    integer, intent(in) :: err
    character(len=80), intent(out) :: out
    character(len=80) :: get_msg
    external get_msg
    out = get_msg(err)
end subroutine

character(len=80) function get_msg(err)
    implicit none
    integer :: err
    get_msg = 'message for error code'
end function

program external_19
    implicit none
    character(len=80) :: out
    call check(1, out)
    if (out /= 'message for error code') error stop
    print *, trim(out)
end program
