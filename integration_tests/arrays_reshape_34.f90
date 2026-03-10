! Test: reshape() with parameter array constructor used in subroutine
! Verifies that module-level parameter arrays initialized via reshape()
! with array constructors referencing other parameters work correctly
! when used as initializers in subroutine local parameters.
module arrays_reshape_34_mod
    implicit none
    integer, parameter :: a(2) = [1, 2]
    integer, parameter :: b(2,2) = reshape([a, a], shape(b))
contains
    subroutine check_reshape_param()
        integer, parameter :: x(2,2) = b
        if (x(1,1) /= 1) error stop
        if (x(2,1) /= 2) error stop
        if (x(1,2) /= 1) error stop
        if (x(2,2) /= 2) error stop
    end subroutine
end module

program arrays_reshape_34
    use arrays_reshape_34_mod
    implicit none
    call check_reshape_param()
    print *, "ok"
end program
