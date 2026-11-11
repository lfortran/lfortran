! A procedure with an array argument declares a local derived type and passes
! an allocatable array component of that type to another procedure.
module derived_types_259_mod
    implicit none
contains
    subroutine fill(col)
        real, intent(out) :: col(:)
        col = 1.0
    end subroutine fill

    subroutine build(x)
        real, intent(in) :: x(:)
        type :: jac_type
            real, allocatable :: jac(:)
        end type jac_type
        type(jac_type), allocatable :: cols(:)
        allocate(cols(1))
        allocate(cols(1)%jac(size(x)))
        call fill(cols(1)%jac)
        if (abs(cols(1)%jac(1) - 1.0) > 1.0e-6) error stop
    end subroutine build
end module derived_types_259_mod

program derived_types_259
    use derived_types_259_mod
    implicit none
    real :: x(2)
    x = 0.0
    call build(x)
end program derived_types_259
