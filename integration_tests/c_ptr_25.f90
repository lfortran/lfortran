! A type(c_ptr) or type(c_funptr), intent(in), TARGET dummy argument of a
! Fortran procedure is associated with its actual argument (F2018 15.5.2.4):
! c_loc of the dummy is the address of the actual, and a pointer associated
! with the dummy stays associated with the actual after the call.
module c_ptr_25_mod
use iso_c_binding, only: c_ptr, c_funptr, c_loc, c_funloc, c_associated, &
    c_f_pointer, c_intptr_t, c_int, c_null_ptr
implicit none

type(c_ptr), target :: a
type(c_ptr), pointer :: pp

type :: holder
    type(c_ptr) :: p
contains
    procedure :: addr => holder_addr
end type

abstract interface
    subroutine addr_iface(p, r)
    import :: c_ptr, c_intptr_t
    type(c_ptr), intent(in), target :: p
    integer(c_intptr_t), intent(out) :: r
    end subroutine
end interface

contains

    subroutine s(p)
    type(c_ptr), intent(in), target :: p
    if (transfer(c_loc(p), 0_c_intptr_t) /= transfer(c_loc(a), 0_c_intptr_t)) &
        error stop 1
    pp => p
    end subroutine

    subroutine addr(p, r)
    type(c_ptr), intent(in), target :: p
    integer(c_intptr_t), intent(out) :: r
    r = transfer(c_loc(p), r)
    end subroutine

    subroutine addr_fwd(p, r)
    type(c_ptr), intent(in), target :: p
    integer(c_intptr_t), intent(out) :: r
    call addr(p, r)
    if (r /= transfer(c_loc(p), r)) r = -1
    end subroutine

    integer(c_intptr_t) function holder_addr(self, p) result(r)
    class(holder), intent(in) :: self
    type(c_ptr), intent(in), target :: p
    r = transfer(c_loc(p), r)
    if (.not. c_associated(self%p, p)) r = -1
    end function

    subroutine addr_optional(r, p)
    integer(c_intptr_t), intent(out) :: r
    type(c_ptr), intent(in), target, optional :: p
    if (present(p)) then
        r = transfer(c_loc(p), r)
    else
        r = -2
    end if
    end subroutine

    subroutine addr_funptr(f, r)
    type(c_funptr), intent(in), target :: f
    integer(c_intptr_t), intent(out) :: r
    r = transfer(c_loc(f), r)
    end subroutine

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

    integer(c_int) function get_bindc_value(p) result(r) bind(c)
    type(c_ptr), value :: p
    r = get_in(p)
    end function

    integer(c_int) function get_bindc_in(p) result(r) bind(c)
    type(c_ptr), intent(in) :: p
    r = get_in(p)
    end function

    integer function get_target(p) result(r)
    type(c_ptr), intent(in), target :: p
    type(c_ptr), pointer :: q
    q => p
    r = get_in(q)
    end function

    ! A TARGET dummy forwarded to dummies passed in every other way.
    integer function forward_target(p) result(r)
    type(c_ptr), intent(in), target :: p
    type(c_ptr) :: q
    r = 0
    if (get_in(p) /= 7) r = 1
    if (get_value(p) /= 7) r = 2
    if (get_bindc_value(p) /= 7) r = 3
    if (get_bindc_in(p) /= 7) r = 4
    if (get_target(p) /= 7) r = 5
    q = p
    if (.not. c_associated(q, p)) r = 6
    if (.not. c_associated(p)) r = 7
    end function

    ! A dummy without TARGET forwarded to a TARGET dummy.
    integer function forward_in(p) result(r)
    type(c_ptr), intent(in) :: p
    r = get_target(p)
    end function

end module

program c_ptr_25
use c_ptr_25_mod
implicit none
integer, target :: x = 3, j = 7
integer(c_intptr_t) :: r
type(c_ptr), target :: b, arr(3)
type(holder), target :: h
type(c_funptr), target :: fptr
procedure(addr_iface), pointer :: fp

! The program of issue #14405.
a = c_loc(x)
call s(a)
if (transfer(c_loc(pp), 0_c_intptr_t) /= transfer(c_loc(a), 0_c_intptr_t)) &
    error stop 2
if (get_in(pp) /= 3) error stop 3

b = c_loc(j)
call addr(b, r)
if (r /= transfer(c_loc(b), 0_c_intptr_t)) error stop 4
call addr_fwd(b, r)
if (r /= transfer(c_loc(b), 0_c_intptr_t)) error stop 5
arr = c_null_ptr
arr(2) = c_loc(j)
call addr(arr(2), r)
if (r /= transfer(c_loc(arr(2)), 0_c_intptr_t)) error stop 6
h%p = c_loc(j)
call addr(h%p, r)
if (r /= transfer(c_loc(h%p), 0_c_intptr_t)) error stop 7
if (h%addr(h%p) /= transfer(c_loc(h%p), 0_c_intptr_t)) error stop 8
fp => addr
call fp(b, r)
if (r /= transfer(c_loc(b), 0_c_intptr_t)) error stop 9
call addr_optional(r, b)
if (r /= transfer(c_loc(b), 0_c_intptr_t)) error stop 10
call addr_optional(r)
if (r /= -2) error stop 11
fptr = c_funloc(get_bindc_value)
call addr_funptr(fptr, r)
if (r /= transfer(c_loc(fptr), 0_c_intptr_t)) error stop 12

if (get_target(b) /= 7) error stop 13
if (get_target(c_loc(j)) /= 7) error stop 14
if (forward_target(b) /= 0) error stop 15
if (forward_target(c_loc(j)) /= 0) error stop 16
if (forward_in(b) /= 7) error stop 17
if (forward_in(c_loc(j)) /= 7) error stop 18
print *, "ok"
end program
