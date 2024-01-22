module testdrive_derived_types_33
    implicit none
    private

    public :: error_type, test_skipped

    type :: error_type
        integer :: stat
        character(len=:), allocatable :: message
    end type error_type

contains

pure function test_skipped(error) result(is_skipped)
    type(error_type), intent(in), optional :: error
    logical :: is_skipped

    is_skipped = .false.
    if (present(error)) then
      is_skipped = error%stat == 1
    end if

  end function test_skipped

end module testdrive_derived_types_33


program main
use testdrive_derived_types_33
implicit none

type(error_type), allocatable :: error_obj

allocate(error_obj)
error_obj%stat = 1
print *, test_skipped(error_obj)
print *, error_obj%stat

end program
