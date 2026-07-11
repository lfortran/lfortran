module procedure_pointer_27_utils_mod
    implicit none
contains
    subroutine default_convert(x, y)
        integer, intent(in) :: x
        integer, intent(out) :: y
        y = x * 3
    end subroutine
end module

module procedure_pointer_27_core_mod
    use procedure_pointer_27_utils_mod, only: default_convert
    implicit none

    abstract interface
        subroutine convert_sub(x, y)
            integer, intent(in) :: x
            integer, intent(out) :: y
        end subroutine
    end interface

    type :: core_type
        procedure(convert_sub), nopass, pointer :: default_convert => default_convert
        integer :: n = 0
    end type

contains

    subroutine dispatch(s, x, y)
        class(core_type), intent(inout) :: s
        integer, intent(in) :: x
        integer, intent(out) :: y
        call s%default_convert(x, y)
    end subroutine
end module

program procedure_pointer_27
    use procedure_pointer_27_core_mod
    implicit none
    type(core_type) :: s
    integer :: y

    call dispatch(s, 5, y)
    if (y /= 15) error stop
end program procedure_pointer_27