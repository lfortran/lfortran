! type(c_ptr), intent(in) dummy arguments without VALUE of Fortran
! procedures: module, internal, recursive, elemental, type-bound and
! procedure-pointer calls, optional dummies, and forwarding to other dummies.
! type(c_funptr), intent(in) dummy arguments follow the same rule.
module c_ptr_25_mod
use iso_c_binding, only: c_ptr, c_null_ptr, c_loc, c_associated, &
    c_f_pointer, c_int, c_intptr_t, c_funptr, c_null_funptr, c_funloc, &
    c_f_procpointer
implicit none

type :: holder
    type(c_ptr) :: p
contains
    procedure :: get => holder_get
    procedure, nopass :: get_nopass => get_in
end type

abstract interface
    integer function get_iface(p)
    import :: c_ptr
    type(c_ptr), intent(in) :: p
    end function

    integer(c_int) function scale_iface(x) bind(c)
    import :: c_int
    integer(c_int), value :: x
    end function
end interface

contains

    integer function get_in(p) result(r)
    type(c_ptr), intent(in) :: p
    integer, pointer :: ip
    if (.not. c_associated(p)) then
        r = -1
        return
    end if
    call c_f_pointer(p, ip)
    r = ip
    end function

    integer function get_value(p) result(r)
    type(c_ptr), value :: p
    r = get_in(p)
    end function

    integer function get_inout(p) result(r)
    type(c_ptr), intent(inout) :: p
    r = get_in(p)
    end function

    integer function get_unspec(p) result(r)
    type(c_ptr) :: p
    r = get_in(p)
    end function

    integer function get_unspec_in(p) result(r)
    type(c_ptr), intent(in) :: p
    type(c_ptr) :: q
    q = p
    r = get_unspec(q)
    end function

    integer(c_int) function get_bindc_value(p) result(r) bind(c)
    type(c_ptr), value :: p
    r = get_in(p)
    end function

    integer(c_int) function get_bindc_in(p) result(r) bind(c)
    type(c_ptr), intent(in) :: p
    r = get_in(p)
    end function

    integer function holder_get(self, p) result(r)
    class(holder), intent(in) :: self
    type(c_ptr), intent(in) :: p
    r = get_in(self%p) + get_in(p)
    end function

    integer function get_optional(p) result(r)
    type(c_ptr), intent(in), optional :: p
    if (present(p)) then
        r = get_in(p)
    else
        r = -2
    end if
    end function

    integer function forward_optional(p) result(r)
    type(c_ptr), intent(in), optional :: p
    r = get_optional(p)
    end function

    integer function forward_all(p) result(r)
    type(c_ptr), intent(in) :: p
    type(c_ptr) :: q
    r = 0
    if (get_in(p) /= 7) r = 1
    if (get_value(p) /= 7) r = 2
    if (get_bindc_value(p) /= 7) r = 3
    if (get_bindc_in(p) /= 7) r = 4
    if (get_optional(p) /= 7) r = 5
    q = p
    if (get_inout(q) /= 7) r = 6
    if (get_unspec(q) /= 7) r = 7
    if (.not. c_associated(q, p)) r = 8
    if (.not. c_associated(p, q)) r = 9
    if (.not. c_associated(p)) r = 10
    end function

    recursive integer function count_down(p, n) result(r)
    type(c_ptr), intent(in) :: p
    integer, intent(in) :: n
    if (n == 0) then
        r = get_in(p)
    else
        r = count_down(p, n - 1) + 1
    end if
    end function

    elemental integer function is_set(p) result(r)
    type(c_ptr), intent(in) :: p
    r = 0
    if (c_associated(p)) r = 1
    end function

    type(c_ptr) function copy_ptr(p) result(r)
    type(c_ptr), intent(in) :: p
    r = p
    end function

    integer function other_uses(p) result(r)
    type(c_ptr), intent(in) :: p
    type(holder) :: h
    type(c_ptr) :: a(2)
    integer(c_intptr_t) :: ia, ib
    integer, pointer :: arr(:)
    r = 0
    h = holder(p)
    if (get_in(h%p) /= 7) r = 1
    h%p = p
    if (get_in(h%p) /= 7) r = 2
    a = [p, c_null_ptr]
    if (get_in(a(1)) /= 7) r = 3
    if (get_in(a(2)) /= -1) r = 4
    ia = transfer(p, ia)
    ib = transfer(h%p, ib)
    if (ia /= ib) r = 5
    associate (q => p)
        if (get_in(q) /= 7) r = 6
    end associate
    if (host_get() /= 7) r = 7
    call c_f_pointer(p, arr, [1])
    if (arr(1) /= 7) r = 8
    if (.not. c_associated(p, h%p)) r = 9
    contains
        integer function host_get()
        host_get = get_in(p)
        end function
    end function

    integer function deref_target(p) result(r)
    type(c_ptr), intent(in), target :: p
    type(c_ptr), pointer :: pp
    call c_f_pointer(c_loc(p), pp)
    r = get_in(pp)
    end function

    integer(c_int) function twice(x) result(r) bind(c)
    integer(c_int), value :: x
    r = 2*x
    end function

    integer(c_int) function thrice(x) result(r) bind(c)
    integer(c_int), value :: x
    r = 3*x
    end function

    integer function call_funptr(f, x) result(r)
    type(c_funptr), intent(in) :: f
    integer, intent(in) :: x
    procedure(scale_iface), pointer :: sp
    if (.not. c_associated(f)) then
        r = -1
        return
    end if
    call c_f_procpointer(f, sp)
    r = sp(int(x, c_int))
    end function

    integer function forward_funptr(f, x) result(r)
    type(c_funptr), intent(in) :: f
    integer, intent(in) :: x
    type(c_funptr) :: g
    r = 0
    if (call_funptr(f, x) /= 2*x) r = 1
    g = f
    if (call_funptr(g, x) /= 2*x) r = 2
    if (.not. c_associated(f, g)) r = 3
    if (.not. c_associated(f, c_funloc(twice))) r = 4
    if (c_associated(f, c_funloc(thrice))) r = 5
    end function

    subroutine set_from(p, q)
    type(c_ptr), intent(in) :: p
    type(c_ptr), intent(out) :: q
    q = p
    end subroutine

end module

program c_ptr_25
use c_ptr_25_mod
implicit none
integer, target :: i = 42, j = 7
type(c_ptr) :: p, q, arr(3)
type(holder) :: h
procedure(get_iface), pointer :: fp
type(c_funptr) :: fptr
integer :: k

p = c_loc(j)
if (get_in(p) /= 7) error stop 1
if (get_in(c_loc(i)) /= 42) error stop 2
if (get_in(c_null_ptr) /= -1) error stop 3
if (forward_all(p) /= 0) error stop 4
if (forward_all(c_loc(j)) /= 0) error stop 5

k = 3
arr = c_null_ptr
arr(k) = c_loc(i)
if (get_in(arr(k)) /= 42) error stop 6
if (forward_all(copy_ptr(p)) /= 0) error stop 7
h%p = c_loc(i)
if (get_in(h%p) /= 42) error stop 8
if (h%get(p) /= 49) error stop 9
if (h%get(h%p) /= 84) error stop 10
if (h%get_nopass(p) /= 7) error stop 11

fp => get_in
if (fp(p) /= 7) error stop 12
if (fp(c_loc(i)) /= 42) error stop 13
fp => get_unspec_in
if (fp(p) /= 7) error stop 14

if (get_optional(p) /= 7) error stop 15
if (get_optional() /= -2) error stop 16
if (forward_optional(p) /= 7) error stop 17
if (forward_optional() /= -2) error stop 18

if (count_down(p, 3) /= 10) error stop 19
if (any(is_set(arr) /= [0, 0, 1])) error stop 20
if (is_set(p) /= 1) error stop 21

q = copy_ptr(p)
if (.not. c_associated(q, c_loc(j))) error stop 22
call set_from(c_loc(i), q)
if (.not. c_associated(q, c_loc(i))) error stop 23
call set_from(arr(1), q)
if (c_associated(q)) error stop 24

if (internal_get(p) /= 7) error stop 25
if (internal_get(c_loc(i)) /= 42) error stop 26
if (other_uses(p) /= 0) error stop 27
if (other_uses(c_loc(j)) /= 0) error stop 28
if (deref_target(p) /= 7) error stop 29

fptr = c_funloc(twice)
if (call_funptr(fptr, 5) /= 10) error stop 30
if (call_funptr(c_funloc(thrice), 5) /= 15) error stop 31
if (call_funptr(c_null_funptr, 5) /= -1) error stop 32
if (forward_funptr(fptr, 4) /= 0) error stop 33
if (forward_funptr(c_funloc(twice), 6) /= 0) error stop 34
print *, "ok"

contains

    integer function internal_get(p) result(r)
    type(c_ptr), intent(in) :: p
    r = get_in(p)
    if (.not. c_associated(p)) r = -3
    end function

end program
