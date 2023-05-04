module derived_types_08_fpm_error
implicit none

    type :: error_t
        character(len=:), allocatable :: message
    end type error_t

contains

    function bad_name_error(error, label, name)
        type(error_t), allocatable, intent(out) :: error
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: name
        logical :: bad_name_error

        allocate(error)
        ! allocate(character(len=4) :: error%message) ! TODO: Should happen automatically in AST -> ASR transition
        error%message = label//"_"//name
        ! bad_name_error = allocated(error%message)

    end function bad_name_error

end module derived_types_08_fpm_error

program derived_types_08
use derived_types_08_fpm_error
implicit none

type(error_t), allocatable :: error

print *, bad_name_error(error, "1", "x")
print *, error%message

end program
