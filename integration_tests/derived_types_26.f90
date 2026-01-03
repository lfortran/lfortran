module derived_types_26_fpm_error_module
implicit none

    type :: error_t
        character(len=:), allocatable :: message
        integer :: z
    end type error_t

contains

    function bad_name_error(error, label, name)
        type(error_t), allocatable, intent(out) :: error
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: name
        integer :: bad_name_error


        allocate(error)
        ! allocate(character(len=4) :: error%message) ! TODO: Should happen automatically in AST -> ASR transition
        error%message = label//"_"//name
        error%z = 20
        bad_name_error = 101
    end function bad_name_error

    subroutine bad_name_error2()


        !> Instance of the error data
        type(error_t), allocatable:: error


        logical :: check
        integer :: e
        check = .false.
        if (.not.check) then
            allocate(error)
            e = 404
            error%message = 'there is an error'
            error%z = 10
        else
            allocate(error)
            e = 0
            error%message = 'there is not an error'
            error%z = 20
        end if


        if (e /= 404) error stop
        if (error%z /= 10) error stop
        print *, error%message


    end subroutine bad_name_error2

end module derived_types_26_fpm_error_module


program derived_types_26
use derived_types_26_fpm_error_module
implicit none


type(error_t), allocatable :: error
integer :: ret

ret =  bad_name_error(error, "1", "x")
if (ret /= 101) error stop
print *, error%message
if (error%z /= 20) error stop
call bad_name_error2()

end program
