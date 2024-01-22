module testdrive_derived_types_30
    implicit none

    public :: unittest_type

    abstract interface
        subroutine test_interface(error)
        integer, intent(out) :: error
        end subroutine test_interface
    end interface


    type :: unittest_type
        procedure(test_interface), pointer, nopass :: test => null()
        logical :: should_fail = .false.
    end type unittest_type

contains

    subroutine run_unittest(test_var, error)
        type(unittest_type), intent(in) :: test_var
        integer, intent(inout) :: error

        call test_var%test(error)
    end subroutine run_unittest

    subroutine test_impl1(error)
        integer, intent(out) :: error
        error = 2
    end subroutine test_impl1


end module testdrive_derived_types_30

program derived_types_30
use testdrive_derived_types_30
implicit none

    type(unittest_type) :: var
    integer :: error

    var % test => test_impl
    call run_unittest(var, error)
    print *, error
    if( error /= 1 ) error stop

    var % test => test_impl1
    call run_unittest(var, error)
    print *, error
    if( error /= 2 ) error stop

contains

    subroutine test_impl(error)
        integer, intent(out) :: error
        error = 1
    end subroutine test_impl

end program
