module derived_types_25_fpm_error
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
        error%message = label//"_"//name
        bad_name_error = allocated(error%message)

    end function bad_name_error

end module derived_types_25_fpm_error

program derived_types_25
use derived_types_25_fpm_error
implicit none

type(error_t), allocatable :: error
character(len=:), allocatable :: message

print *, allocated(message)
if( allocated(message) ) error stop

if( .not. bad_name_error(error, "1", "x") ) error stop
print *, error%message
if( error%message /= "1_x" ) error stop

end program
