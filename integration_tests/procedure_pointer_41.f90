! A procedure pointer component of a named constant cannot be called
! directly: a procedure component reference is
! `scalar-variable % procedure-component-name` (Fortran 2023 R1040), and a
! variable shall not be a constant or a subobject of a constant (C901). The
! procedure is called through a variable copy of the constant, or through a
! dummy argument associated with the constant, an element of a constant
! array or a whole constant array, which is a variable. A type-bound
! procedure of a constant, `data-ref % binding-name` (R1522), and the data
! components of a constant remain valid.
module procedure_pointer_41_funcs
implicit none
abstract interface
    integer function ifunc(x)
        integer, intent(in) :: x
    end function
    subroutine isub(x)
        integer, intent(inout) :: x
    end subroutine
end interface
contains
    integer function inc(x)
        integer, intent(in) :: x
        inc = x + 1
    end function
    subroutine add_ten(x)
        integer, intent(inout) :: x
        x = x + 10
    end subroutine
end module

module procedure_pointer_41_mod
use procedure_pointer_41_funcs
implicit none
type :: pc
    procedure(ifunc), pointer, nopass :: f => null()
    procedure(isub), pointer, nopass :: sp => null()
    integer :: n = 5
contains
    procedure, nopass :: m => tb_func
    procedure, nopass :: ms => tb_sub
end type
type :: outer
    type(pc) :: inner
end type
type(pc), parameter :: first = pc(inc, add_ten, 3)
type(outer), parameter :: o = outer(pc(inc, add_ten, 4))
type(pc), parameter :: os(2) = [pc(inc, add_ten, 1), pc(inc, add_ten, 2)]
contains
    integer function tb_func(x)
        integer, intent(in) :: x
        tb_func = 2*x
    end function
    subroutine tb_sub(x)
        integer, intent(inout) :: x
        x = 3*x
    end subroutine
    integer function use_x(x)
        type(pc), intent(in) :: x
        integer :: r
        r = 1
        call x%sp(r)
        use_x = x%f(r)
    end function
    integer function use_all(xs)
        type(pc), intent(in) :: xs(:)
        integer :: i
        use_all = 0
        do i = 1, size(xs)
            use_all = use_all + xs(i)%f(xs(i)%n)
        end do
    end function
end module

program procedure_pointer_41
use procedure_pointer_41_mod
implicit none
type(pc) :: v
type(outer) :: w
type(pc), target :: tv
type(pc), pointer :: pp
procedure(ifunc), pointer :: q
integer :: r
v = first
w = o
r = 1
call v%sp(r)
if (r /= 11) error stop
if (v%f(r) /= 12) error stop
if (w%inner%f(11) /= 12) error stop
if (use_x(first) /= 12) error stop
if (use_x(o%inner) /= 12) error stop
if (use_x(os(2)) /= 12) error stop
if (use_all(os) /= 5) error stop
if (first%m(4) /= 8) error stop
if (o%inner%m(5) /= 10) error stop
if (os(1)%m(6) /= 12) error stop
r = 1
call first%ms(r)
if (r /= 3) error stop
call o%inner%ms(r)
if (r /= 9) error stop
tv = os(1)
pp => tv
r = 1
call pp%sp(r)
if (pp%f(r) /= 12) error stop
q => v%f
if (q(1) /= 2) error stop
if (.not. associated(q, w%inner%f)) error stop
if (first%n /= 3) error stop
if (o%inner%n /= 4) error stop
if (os(2)%n /= 2) error stop
print *, "ok"
end program
