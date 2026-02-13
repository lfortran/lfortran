module lfortran_intrinsic_iso_c_binding
implicit none

type :: c_ptr
    integer ptr
end type

type :: c_funptr
    integer ptr
end type

integer, parameter :: c_int8_t = 1
integer, parameter :: c_int16_t = 2
integer, parameter :: c_int32_t = 4
integer, parameter :: c_int64_t = 8
integer, parameter :: c_int = 4
integer, parameter :: c_short = 2
integer, parameter :: c_long = 8
integer, parameter :: c_long_long = 8
integer, parameter :: c_size_t = 8
integer, parameter :: c_float = 4
integer, parameter :: c_double = 8
integer, parameter :: c_long_double = -1
integer, parameter :: c_float_complex = 4
integer, parameter :: c_double_complex = 8
integer, parameter :: c_long_double_complex = -1
integer, parameter :: c_bool = 1
integer, parameter :: c_char = 1
integer, parameter :: c_intptr_t = 8
character(len=1), parameter :: c_null_char = char(0)
character(len=1), parameter :: c_new_line = char(10)
type(c_ptr), parameter :: c_null_ptr = c_ptr(0)
type(c_funptr), parameter :: c_null_funptr = c_funptr(0)

interface
    logical function c_associated(c_ptr_1)
    import c_ptr
    type(c_ptr), intent(in) :: c_ptr_1
    end function

    subroutine c_f_pointer(cptr, fptr, shape)
    import c_ptr
    type(c_ptr), intent(in) :: cptr
    !type(*), pointer, intent(out) :: fptr
    integer, pointer, intent(out) :: fptr
    integer, intent(in), optional :: shape(:)
    end subroutine

    subroutine c_f_procpointer(cptr, fptr)
    import c_funptr
    type(c_funptr), intent(in) :: cptr
    !procedure(*), pointer, intent(out) :: fptr
    integer, pointer, intent(out) :: fptr
    end subroutine

    !type(c_ptr) function c_loc(x)
    integer function c_loc(x)
    import c_ptr
    !type(*), intent(in) :: x
    integer, intent(in) :: x
    end function

    !type(c_funptr) function c_funloc(x)
    integer function c_funloc(x)
    import c_funptr
    !type(*), intent(in) :: x
    integer, intent(in) :: x
    end function
end interface

end module
