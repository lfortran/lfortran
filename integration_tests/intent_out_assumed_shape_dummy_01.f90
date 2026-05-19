! Issue #11563: lfortran should not raise a runtime shape-mismatch
! error when the LHS of an assignment is a non-allocatable assumed-shape
! intent(out) dummy argument. The shape is fixed by the caller and a
! shape mismatch there is undefined behavior per the Fortran standard,
! but gfortran's default (without -fcheck=bounds) silently accepts it.
! This test only verifies that the program reaches the end without
! aborting; it does not inspect the contents of the dummy after the
! mismatched assignment (which would be UB).
module m_intent_out_assumed_shape_dummy_01
    use, intrinsic :: iso_fortran_env, only: real32
    implicit none

    type :: array_type
        real(real32), dimension(:,:), allocatable :: val
    contains
        procedure, pass(this) :: write_partial
    end type array_type

contains

    subroutine write_partial(this, upstream_grad, output)
        class(array_type), intent(in) :: this
        real(real32), dimension(:,:), intent(in) :: upstream_grad
        real(real32), dimension(:,:), intent(out) :: output
        output = upstream_grad * 2.0_real32
    end subroutine write_partial

end module m_intent_out_assumed_shape_dummy_01


program intent_out_assumed_shape_dummy_01
    use, intrinsic :: iso_fortran_env, only: real32
    use m_intent_out_assumed_shape_dummy_01
    implicit none

    type(array_type) :: a
    real(real32) :: ug(8, 1)
    real(real32), allocatable :: out(:,:)

    allocate(a%val(8, 1))
    allocate(out(1, 1))
    ug = 1.0_real32
    out = 0.0_real32

    call a%write_partial(ug, out)

    if (size(out, 1) /= 1) error stop 1
    if (size(out, 2) /= 1) error stop 2

    print *, "PASS"
end program intent_out_assumed_shape_dummy_01
