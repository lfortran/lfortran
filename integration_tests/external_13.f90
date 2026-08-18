! Test that external functions with implicit interfaces get underscore mangling
! when --mangle-underscore flag is used.
!
! This test verifies the fix for issue #9473 where --mangle-underscore and
! --all-mangling did not apply to external interface functions (like LAPACK/BLAS).
!
! We define a simple stub to avoid needing actual LAPACK at link time.

module external_mangle_stubs
    implicit none
contains
    subroutine dummy_external_(x) bind(C, name="dummy_external_")
        integer, intent(inout) :: x
        x = x + 1
    end subroutine
end module

program external_mangle_01
    use external_mangle_stubs
    implicit none
    integer :: x

    x = 41

    ! Call external function with implicit interface
    ! With --mangle-underscore, this should generate a call to dummy_external_
    call dummy_external(x)

    if (x == 42) then
        print *, "PASSED"
    else
        print *, "FAILED: x =", x
        error stop
    end if
end program
