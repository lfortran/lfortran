module derived_types_225_m
implicit none

type :: s
    integer :: i = 0
end type

type, extends(s) :: s2
    integer :: j = 0
end type

type :: t
    class(s), pointer :: p => null()
    class(*), pointer :: u => null()
end type

class(s), pointer :: mp => null()

contains

subroutine v_null(v)
    type(t), value :: v
    class(s), pointer :: e
    v%p => null()
    v%u => null()
    if (associated(v%p) .or. associated(v%u)) error stop 1
    e => null()
    v%p => e
    if (associated(v%p)) error stop 2
end subroutine

subroutine v_alloc(v, w)
    type(t), value :: v
    class(s), intent(in) :: w
    allocate(v%p)
    v%p%i = 5
    deallocate(v%p)
    allocate(v%p, source=s(6))
    if (v%p%i /= 6) error stop 3
    deallocate(v%p)
    allocate(v%p, source=w)
    if (v%p%i /= w%i) error stop 4
    deallocate(v%p)
    allocate(integer :: v%u)
    deallocate(v%u)
end subroutine

subroutine l_null_alloc(x, n)
    type(s), target, intent(inout) :: x
    integer, target, intent(in) :: n
    type(t) :: b
    class(s), pointer :: w
    class(*), pointer :: u
    b%p => x
    b%p => null()
    if (associated(b%p)) error stop 5
    b%p => x
    allocate(b%p)
    b%p%i = 8
    if (x%i /= 3) error stop 6
    deallocate(b%p)
    b%u => n
    b%u => null()
    b%u => n
    allocate(integer :: b%u)
    deallocate(b%u)
    w => x
    w => null()
    w => x
    allocate(w)
    if (x%i /= 3) error stop 7
    deallocate(w)
    u => n
    u => null()
    if (associated(u)) error stop 8
end subroutine

subroutine clear(p)
    class(s), pointer, intent(inout) :: p
    p => null()
end subroutine

end module

program derived_types_225
use derived_types_225_m
implicit none
type(s), target :: x
type(s2) :: y
integer, target :: n = 7
type(t) :: a
class(s), pointer :: q => null()
integer :: k
x%i = 3
y%i = 4
a%p => x
a%u => n
do k = 1, 20
    call v_null(a)
    call v_alloc(a, y)
    call l_null_alloc(x, n)
    q => x
    call clear(q)
    if (associated(q)) error stop 9
    mp => x
    call clear(mp)
    if (associated(mp)) error stop 10
    q => x
    q => null()
    mp => x
    mp => null()
end do
if (.not. associated(a%p, x)) error stop 11
if (.not. associated(a%u)) error stop 12
if (x%i /= 3) error stop 13
print *, "ok"
end program
