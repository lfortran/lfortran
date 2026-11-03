! The final procedure of a local of a host invokes a procedure value that
! the host made, an internal procedure of the host, while another
! declaration of the host inquires about the bounds of that local: a local,
! a dummy argument and an interface body that imports it. The procedure
! value reads only local scalars of the host, since the order in which the locals
! of the host are finalized and freed is processor dependent (#16610)
module finalization_58_mod
implicit none
abstract interface
    integer function getter()
    end function
end interface
procedure(getter), pointer :: saved => null()
integer :: seen = 0
type :: t
    integer :: v = 0
contains
    final :: finalize
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x(:)
    if (associated(saved)) seen = saved()
    saved => null()
end subroutine
subroutine host_local(n)
    integer, intent(in) :: n
    type(t) :: x(n:2*n-1)
    integer :: m(lbound(x, 1):ubound(x, 1))
    integer :: w(size(x) + 1)
    integer :: k
    m = 3
    w = 1
    k = n + sum(m) + lbound(m, 1) + 10*size(w)
    saved => get
contains
    integer function get()
        get = k
    end function
end subroutine
subroutine host_dummy(n, y)
    integer, intent(in) :: n
    type(t) :: x(n:2*n-1)
    integer, intent(inout) :: y(lbound(x, 1):ubound(x, 1))
    integer :: k
    k = 2*n
    saved => get
contains
    integer function get()
        get = k
    end function
end subroutine
subroutine host_import(n)
    integer, intent(in) :: n
    type(t) :: x(n:2*n-1)
    interface
        subroutine inner(a)
            import :: x
            integer, intent(inout) :: a(lbound(x, 1):ubound(x, 1))
        end subroutine
    end interface
    integer :: b(n)
    integer :: k
    b = 2
    k = sum(b)
    saved => get
contains
    integer function get()
        get = k
    end function
end subroutine
! An array of zero extent
subroutine host_empty(n)
    integer, intent(in) :: n
    type(t) :: x(n:n-2)
    integer :: w(size(x) + 1)
    integer :: m(lbound(x, 1):ubound(x, 1))
    integer :: k
    w = 5
    k = sum(w) + size(m)
    saved => get
contains
    integer function get()
        get = k
    end function
end subroutine
end module

program finalization_58
use finalization_58_mod
implicit none
integer :: y(3)
call host_local(2)
print *, seen
if (seen /= 2 + 6 + 2 + 30) error stop
seen = 0
y = 2
call host_dummy(3, y)
print *, seen
if (seen /= 6) error stop
seen = 0
call host_import(3)
print *, seen
if (seen /= 6) error stop
seen = 0
call host_empty(4)
print *, seen
if (seen /= 5) error stop
end program
