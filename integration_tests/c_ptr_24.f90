! A type(c_ptr), intent(in) dummy argument without VALUE of a non-bind(c)
! procedure is passed by reference, compatible with GFortran's calling
! convention (checked from C). The same holds for type(c_funptr).
module c_ptr_24_mod
use iso_c_binding, only: c_ptr, c_null_ptr, c_loc, c_associated, &
    c_f_pointer, c_funptr, c_null_funptr, c_funloc, c_int
implicit none

type :: holder
    type(c_ptr) :: p
end type

type(c_ptr) :: mod_p
integer, target :: mod_k = 9

abstract interface
    integer function cptr_f_iface(p)
    import :: c_ptr
    type(c_ptr), intent(in) :: p
    end function
end interface

interface
    integer function cptr_c_get(p)
    import :: c_ptr
    type(c_ptr), intent(in) :: p
    end function

    integer function cptr_c_call(f, x)
    import :: cptr_f_iface
    procedure(cptr_f_iface) :: f
    integer, intent(in) :: x
    end function

    integer function cptr_c_funcall(f, x)
    import :: c_funptr
    type(c_funptr), intent(in) :: f
    integer, intent(in) :: x
    end function
end interface

contains

    integer function cptr_f_get_mod(p) result(r)
    type(c_ptr), intent(in) :: p
    integer, pointer :: ip
    if (.not. c_associated(p)) then
        r = -1
        return
    end if
    call c_f_pointer(p, ip)
    r = ip
    end function

    type(c_ptr) function get_ptr() result(r)
    r = c_loc(mod_k)
    end function

    integer function forward_in(p) result(r)
    type(c_ptr), intent(in) :: p
    r = cptr_c_get(p)
    end function

    integer(c_int) function twice(x) result(r) bind(c)
    integer(c_int), value :: x
    r = 2*x
    end function

    integer function forward_funptr(f, x) result(r)
    type(c_funptr), intent(in) :: f
    integer, intent(in) :: x
    r = cptr_c_funcall(f, x)
    end function

    integer function forward_inout(p) result(r)
    type(c_ptr), intent(inout) :: p
    r = cptr_c_get(p)
    end function

    integer function forward_value(p) result(r)
    type(c_ptr), value :: p
    r = cptr_c_get(p)
    end function

end module

program c_ptr_24
use c_ptr_24_mod
implicit none
integer, target :: i = 42, j = 7
type(c_ptr) :: p, arr(3)
type(holder) :: h
type(c_funptr) :: fptr
integer :: k

if (cptr_c_get(c_loc(i)) /= 42) error stop 1
if (cptr_c_get(c_null_ptr) /= -1) error stop 2
p = c_loc(j)
if (cptr_c_get(p) /= 7) error stop 3
if (.not. c_associated(p, c_loc(j))) error stop 4
mod_p = c_loc(i)
if (cptr_c_get(mod_p) /= 42) error stop 5
k = 2
arr = c_null_ptr
arr(k) = c_loc(j)
if (cptr_c_get(arr(k)) /= 7) error stop 6
if (cptr_c_get(arr(1)) /= -1) error stop 7
h%p = c_loc(i)
if (cptr_c_get(h%p) /= 42) error stop 8
if (cptr_c_get(get_ptr()) /= 9) error stop 9
if (forward_in(p) /= 7) error stop 10
if (forward_inout(p) /= 7) error stop 11
if (forward_value(p) /= 7) error stop 12
if (forward_in(c_null_ptr) /= -1) error stop 13

k = cptr_c_call(cptr_f_get_mod, i)
print *, k
if (k /= 0) error stop 14

fptr = c_funloc(twice)
if (cptr_c_funcall(fptr, 5) /= 10) error stop 15
if (cptr_c_funcall(c_funloc(twice), 6) /= 12) error stop 16
if (cptr_c_funcall(c_null_funptr, 6) /= -1) error stop 17
if (forward_funptr(fptr, 7) /= 14) error stop 18
print *, "ok"
end program
