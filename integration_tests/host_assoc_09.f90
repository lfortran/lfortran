! Procedure values of internal procedures that use host variables: actual
! arguments, procedure pointers and procedure-pointer components. A value
! reaches the variables of the instance of its host that made it, including
! after that instance called the host again.
module host_assoc_09_m
implicit none
abstract interface
    integer function ifn()
    end function ifn
    subroutine iface(k)
        integer, intent(out) :: k
    end subroutine iface
    subroutine cb()
    end subroutine cb
    integer function f_i(x)
        integer, intent(in) :: x
    end function f_i
end interface
type :: t
    procedure(f_i), pointer, nopass :: p => null()
end type t
procedure(ifn), pointer :: fp => null()
procedure(iface), pointer :: callback => null()
procedure(cb), pointer :: saved => null()
integer :: seen_dummy(3) = 0
integer :: seen_pointer(3) = 0
integer :: seen_forward(3) = 0
integer :: closure_depth = 0
integer :: ncalls = 0

contains

integer function call_fp()
    call_fp = fp()
end function call_fp

subroutine call_saved()
    call saved()
end subroutine call_saved

subroutine top(k)
    integer, intent(out) :: k
    k = -1
end subroutine top

subroutine noop()
end subroutine noop

integer function apply(g, x)
    procedure(f_i) :: g
    integer, intent(in) :: x
    apply = g(x)
end function apply

! #13603: a module procedure pointer to an internal procedure, called from
! another module procedure while the host is active
subroutine pointer_to_internal(r)
    integer, intent(out) :: r
    integer :: t
    t = 5
    fp => f
    r = call_fp()
contains
    integer function f()
        f = t
    end function f
end subroutine pointer_to_internal

! #16011: called through a pointer after the host called itself, the
! internal procedure reaches the assumed-shape dummy of its own instance
recursive subroutine after_recursion(depth, a)
    integer, intent(in) :: depth
    integer, intent(inout) :: a(:)
    call inner()
    if (depth > 1) call after_recursion(depth - 1, a)
    saved => inner
    call call_saved()
contains
    subroutine inner()
        a(1) = a(1) + 1
    end subroutine inner
end subroutine after_recursion

! d02: a dummy procedure invoked by a newer instance of the host that made it
recursive subroutine dummy_from_caller(n, p)
    integer, intent(in) :: n
    procedure(iface) :: p
    integer, allocatable :: a(:)
    integer :: m
    allocate(a(2))
    a = 10*n
    m = 10*n
    call p(seen_dummy(n))
    if (n > 1) call dummy_from_caller(n - 1, inner)
contains
    subroutine inner(k)
        integer, intent(out) :: k
        k = a(1) + m
    end subroutine inner
end subroutine dummy_from_caller

! d05: a procedure pointer invoked by a newer instance of the host that
! associated it
recursive subroutine pointer_from_caller(n)
    integer, intent(in) :: n
    integer, allocatable :: a(:)
    integer :: k
    allocate(a(2))
    a = 10*n
    if (associated(callback)) call callback(seen_pointer(n))
    callback => inner
    call inner(k)
    if (k /= 10*n) error stop 1
    if (n > 1) call pointer_from_caller(n - 1)
contains
    subroutine inner(k)
        integer, intent(out) :: k
        k = a(1)
    end subroutine inner
end subroutine pointer_from_caller

! d06: the host passes its internal procedure to a new instance of itself
! from another internal procedure
recursive subroutine forward_from_internal(n, b, p)
    integer, intent(in) :: n
    integer, intent(inout) :: b(:)
    procedure(iface) :: p
    integer, allocatable :: a(:)
    allocate(a(2))
    a = 10*n
    call p(seen_forward(n))
    b(1) = b(1) + 1
    if (n > 1) call inner2()
contains
    subroutine inner(k)
        integer, intent(out) :: k
        k = a(1)
    end subroutine inner
    subroutine inner2()
        call forward_from_internal(n - 1, a, inner)
    end subroutine inner2
end subroutine forward_from_internal

! #15994: the dummy procedure is invoked from an internal procedure of the
! newer instance
recursive subroutine closure_argument(depth, f)
    integer, intent(in) :: depth
    procedure(cb) :: f
    if (depth == 2) then
        call closure_argument(1, inner)
    else
        call helper()
    end if
contains
    subroutine inner()
        closure_depth = depth
    end subroutine inner
    subroutine helper()
        call f()
    end subroutine helper
end subroutine closure_argument

! t24: a pointer associated after a direct call of the internal procedure
subroutine pointer_after_call()
    integer :: k
    integer :: arr(3)
    k = 5
    arr = [1, 2, 3]
    call inner()
    saved => inner
    call call_saved()
    if (ncalls /= 2) error stop 2
contains
    subroutine inner()
        if (k /= 5) error stop 3
        if (any(arr /= [1, 2, 3])) error stop 4
        ncalls = ncalls + 1
    end subroutine inner
end subroutine pointer_after_call

! A procedure pointer whose interface takes the bound of its dummy argument
! from a variable of the host (`import :: n`), associated with an internal
! procedure of the same interface
recursive subroutine imported_bound(depth, total)
    integer, intent(in) :: depth
    integer, intent(inout) :: total
    integer :: n
    abstract interface
        subroutine sub(lb, a)
            import :: n
            integer, intent(in) :: lb
            integer, intent(inout) :: a(lb:lb + n - 1)
        end subroutine sub
    end interface
    procedure(sub), pointer :: ps
    integer :: x(6)
    n = depth
    x = 1
    ps => twice
    if (depth > 1) call imported_bound(depth - 1, total)
    call ps(-depth, x)
    total = total + sum(x)
contains
    subroutine twice(lb, a)
        integer, intent(in) :: lb
        integer, intent(inout) :: a(lb:lb + n - 1)
        if (lbound(a, 1) /= lb .or. size(a) /= n) error stop 23
        a = 2*a
    end subroutine twice
end subroutine imported_bound

end module host_assoc_09_m

program host_assoc_09
use host_assoc_09_m
implicit none
type(t) :: s
integer :: k, r, total
integer, allocatable :: ka(:)
integer :: kf(1)
integer :: a(3), b(2)
integer :: outer_seen(2), nouter

call pointer_to_internal(r)
if (r /= 5) error stop 10

! #15164: a procedure-pointer component targeting an internal procedure of
! the program, passed as an actual argument to a module procedure
k = 2
allocate(ka(1))
ka(1) = 3
kf(1) = 4
s%p => add_k
if (apply(s%p, 3) /= 5) error stop 11
s%p => add_ka
if (apply(s%p, 3) /= 6) error stop 12
s%p => add_kf
if (apply(s%p, 3) /= 7) error stop 13

a = 0
call after_recursion(2, a)
if (any(a /= [4, 0, 0])) error stop 14

call dummy_from_caller(3, top)
if (any(seen_dummy /= [40, 60, -1])) error stop 15

call pointer_from_caller(3)
if (any(seen_pointer /= [20, 30, 0])) error stop 16

b = 0
call forward_from_internal(3, b, top)
if (any(seen_forward /= [20, 30, -1])) error stop 17
if (any(b /= [1, 0])) error stop 18

call closure_argument(2, noop)
if (closure_depth /= 2) error stop 19

call pointer_after_call()

total = 0
call imported_bound(3, total)
if (total /= 24) error stop 20

! #15243: a recursive internal procedure passes itself to a sibling
nouter = 0
call outer(1)
if (nouter /= 2) error stop 21
if (any(outer_seen /= [0, 1])) error stop 22
print *, "ok"

contains

integer function add_k(x)
    integer, intent(in) :: x
    add_k = x + k
end function add_k

integer function add_ka(x)
    integer, intent(in) :: x
    add_ka = x + ka(1)
end function add_ka

integer function add_kf(x)
    integer, intent(in) :: x
    add_kf = x + kf(1)
end function add_kf

recursive subroutine outer(n)
    integer, intent(in) :: n
    if (n > 0) call call_outer(outer, n)
    nouter = nouter + 1
    outer_seen(nouter) = n
end subroutine outer

subroutine call_outer(g, n)
    procedure(outer) :: g
    integer, intent(in) :: n
    call g(n - 1)
end subroutine call_outer

end program host_assoc_09
