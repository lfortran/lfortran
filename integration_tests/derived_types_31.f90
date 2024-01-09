module testdrive_derived_types_31
    use, intrinsic :: iso_fortran_env, only : error_unit
    implicit none
    private

    public :: new_testsuite
    public :: unittest_type, testsuite_type, error_type
    public :: test_interface, collect_interface

    integer, parameter :: success = 0

    type :: error_type
        integer :: stat = success
        character(len=:), allocatable :: message

    contains

        final :: escalate_error

    end type error_type

    abstract interface
        subroutine test_interface(error)
        import :: error_type
        type(error_type), allocatable, intent(out) :: error
        end subroutine test_interface
    end interface

    type :: unittest_type
        character(len=:), allocatable :: name
        procedure(test_interface), pointer, nopass :: test => null()
        logical :: should_fail = .false.
    end type unittest_type

    abstract interface
        subroutine collect_interface(testsuite)
        import :: unittest_type
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        end subroutine collect_interface
    end interface

    type :: testsuite_type
        character(len=:), allocatable :: name
        procedure(collect_interface), pointer, nopass :: collect => null()
    end type testsuite_type

contains

    function new_testsuite(name, collect) result(self)
        character(len=*), intent(in) :: name
        procedure(collect_interface) :: collect
        type(testsuite_type) :: self

        self%name = name
        self%collect => collect
    end function new_testsuite

    subroutine escalate_error(error)

        type(error_type), intent(inout) :: error

        if (error%stat /= success) then
            write(error_unit, '(a)') "[Fatal] Uncaught error"
            if (allocated(error%message)) then
                write(error_unit, '(a, 1x, i0, *(1x, a))') &
                "Code:", error%stat, "Message:", error%message
            end if
            error stop
        end if

    end subroutine escalate_error

end module testdrive_derived_types_31

program main
use testdrive_derived_types_31
implicit none

type(testsuite_type) :: suite
type(unittest_type), allocatable :: array_suite(:)

suite = new_testsuite("name", collect_impl)

call suite%collect(array_suite)

print *, size(array_suite)
if( size(array_suite) /= 10 ) error stop

contains

subroutine collect_impl(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    allocate(testsuite(10))
end subroutine collect_impl

end program
