! Pointer, allocatable and ASSOCIATE-name scalar actuals passed to plain
! dummies of bind(c) procedures (a module procedure and an explicit interface
! with the same binding label), and pointer and allocatable dummies passed on
! to a bind(c) procedure and to an external with an implicit interface.
module implicit_interface_94_m
use iso_c_binding, only: c_int, c_ptr, c_null_ptr
implicit none
type, bind(c) :: pt
    integer(c_int) :: a, b
end type
contains
    subroutine inc(x) bind(c, name="ii94_inc")
        integer(c_int) :: x
        x = x + 1
    end subroutine

    subroutine setpt(p) bind(c, name="ii94_setpt")
        type(pt) :: p
        p%a = p%a + 10
    end subroutine

    subroutine clear(x) bind(c, name="ii94_clear")
        type(c_ptr) :: x
        x = c_null_ptr
    end subroutine
end module

program implicit_interface_94
use iso_c_binding, only: c_int, c_ptr, c_loc, c_associated
use implicit_interface_94_m
implicit none
interface
    subroutine inc2(x) bind(c, name="ii94_inc")
        import :: c_int
        integer(c_int) :: x
    end subroutine
    subroutine viaptr(p, a)
        import :: c_int
        integer(c_int), pointer :: p
        integer(c_int), allocatable :: a
    end subroutine
end interface
integer(c_int), pointer :: p
integer(c_int), allocatable :: al
integer(c_int), target :: t
type(pt), pointer :: pp
type(pt), allocatable :: pa
type(c_ptr), pointer :: cpp
type(c_ptr), target :: ct
integer(c_int) :: arr(3)
integer(c_int), pointer :: q
integer(c_int), allocatable :: b
t = 1
p => t
call inc(p)
call inc2(p)
if (t /= 3) error stop 1
allocate(al)
al = 5
call inc(al)
call inc2(al)
if (al /= 7) error stop 2
allocate(pp)
pp%a = 1
pp%b = 2
call setpt(pp)
if (pp%a /= 11 .or. pp%b /= 2) error stop 3
allocate(pa)
pa%a = 3
pa%b = 4
call setpt(pa)
if (pa%a /= 13 .or. pa%b /= 4) error stop 4
ct = c_loc(t)
cpp => ct
call clear(cpp)
if (c_associated(ct)) error stop 5
arr = 0
associate (e => arr(2))
    call inc(e)
    call inc2(e)
end associate
if (any(arr /= [0, 2, 0])) error stop 6
allocate(q)
q = 0
allocate(b)
b = 100
call viaptr(q, b)
if (q /= 11 .or. b /= 111) error stop 7
print *, t, al, pp%a, pa%a, arr, q, b
deallocate(al, pp, pa, q, b)
end program

subroutine viaptr(p, a)
use iso_c_binding, only: c_int
use implicit_interface_94_m, only: inc
implicit none
integer(c_int), pointer :: p
integer(c_int), allocatable :: a
external :: seti
call inc(p)
call seti(p)
call inc(a)
call seti(a)
end subroutine

subroutine seti(i)
integer :: i
i = i + 10
end subroutine
