module c_f_proc_ptr_02_module
use iso_c_binding
implicit none
abstract interface
    integer(c_int) function callback(x) bind(c)
        import c_int
        integer(c_int), value :: x
    end function
end interface
contains
integer(c_int) function twice(x) bind(c)
    integer(c_int), value :: x
    twice = x * 2
end function
function export(f) result(address)
    procedure(callback) :: f
    type(c_funptr) :: address
    address = c_funloc(f)
end function
subroutine invoke_c(f, x, result) bind(c)
    procedure(callback) :: f
    integer(c_int), value :: x
    integer(c_int), intent(out) :: result
    result = f(x)
end subroutine
subroutine invoke_c_nested(f, x, result) bind(c)
    procedure(callback) :: f
    integer(c_int), intent(in) :: x
    integer(c_int), intent(out) :: result
    call inner()
contains
    subroutine inner()
        result = f(x)
    end subroutine
end subroutine
end module
program c_f_proc_ptr_02
use c_f_proc_ptr_02_module
implicit none
type :: holder
    type(c_funptr) :: address
end type
procedure(callback), pointer :: p, copied
type(c_funptr) :: address
type(holder) :: box
integer(c_int) :: result
address = export(twice)
if (.not. c_associated(address, c_funloc(twice))) error stop 1
box%address = address
call c_f_procpointer(box%address, p)
if (.not. associated(p)) error stop 2
if (p(5_c_int) /= 10) error stop 3
copied => p
if (.not. associated(p, copied)) error stop 4
if (.not. c_associated(export(copied), address)) error stop 5
call invoke_c(copied, 7_c_int, result)
if (result /= 14) error stop 6
call invoke_c_nested(copied, 8_c_int, result)
if (result /= 16) error stop 7
call c_f_procpointer(c_null_funptr, p)
if (associated(p)) error stop 8
if (copied(9_c_int) /= 18) error stop 9
end program
