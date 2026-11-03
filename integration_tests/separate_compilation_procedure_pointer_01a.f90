! Module procedures that call through procedure pointer components with an
! implicit interface, a function and a subroutine, used from another file.
module separate_compilation_procedure_pointer_01a_mod
implicit none
type :: holder
    procedure(integer), pointer, nopass :: f => null()
    procedure(), pointer, nopass :: s => null()
end type
contains
integer function inc(x)
    integer, intent(in) :: x
    inc = x + 1
end function

subroutine twice(x, y)
    integer, intent(in) :: x
    integer, intent(out) :: y
    y = 2*x
end subroutine

subroutine set(o)
    type(holder), intent(inout) :: o
    o%f => inc
    o%s => twice
end subroutine

integer function call_f(o, x)
    type(holder), intent(in) :: o
    integer, intent(in) :: x
    call_f = o%f(x)
end function

integer function call_s(o, x)
    type(holder), intent(in) :: o
    integer, intent(in) :: x
    call o%s(x, call_s)
end function
end module
