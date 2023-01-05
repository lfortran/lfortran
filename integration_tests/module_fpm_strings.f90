module fpm_strings
use iso_c_binding, only: c_char, c_ptr, c_size_t
implicit none

public :: f_string, string_t

type string_t
    character(len=:), allocatable :: s
end type

interface f_string
    module procedure f_string, f_string_cptr, f_string_cptr_n
end interface f_string

contains

function f_string(c_string)
    character(len=1), intent(in) :: c_string(:)
    character(:), allocatable :: f_string
end function f_string

function f_string_cptr(cptr) result(s)
    type(c_ptr), intent(in), value :: cptr
    character(len=:, kind=c_char), allocatable :: s

    interface
        function c_strlen(s) result(r) bind(c, name="strlen")
            import c_size_t, c_ptr
            type(c_ptr), intent(in), value :: s
            integer(kind=c_size_t) :: r
        end function
    end interface

    s = f_string_cptr_n(cptr, c_strlen(cptr))
end function

function f_string_cptr_n(cptr, n) result(s)
    type(c_ptr), intent(in), value :: cptr
    integer(kind=c_size_t), intent(in) :: n
    character(len=n, kind=c_char) :: s
end function

end module fpm_strings
