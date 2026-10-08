module derived_types_221_mod
implicit none
type :: s
    integer :: i
end type
type, extends(s) :: s2
    integer :: j
end type
type :: t
    class(s), pointer :: p => null()
end type
type :: u
    class(*), pointer :: q => null()
end type
contains

subroutine check_scalar_copy(x, y)
    type(s), target, intent(in) :: x, y
    type(t) :: a, b
    a%p => x
    b = a
    if (.not. associated(b%p, x)) error stop
    b%p => y
    if (.not. associated(a%p, x)) error stop
    if (a%p%i /= 1) error stop
    if (b%p%i /= 2) error stop
end subroutine

subroutine check_dynamic_type(z)
    type(s2), target, intent(in) :: z
    type(t) :: a, b
    a%p => z
    b = a
    select type (q => b%p)
    type is (s2)
        if (q%j /= 30) error stop
    class default
        error stop
    end select
    nullify(a%p)
    if (.not. associated(b%p, z)) error stop
    b = a
    if (associated(b%p)) error stop
end subroutine

subroutine check_array_constructor(x)
    type(s), target, intent(in) :: x
    class(s), pointer :: w
    type(t) :: a(2)
    type(t), allocatable :: c(:)
    w => x
    a = [t(w), t(w)]
    if (.not. associated(a(1)%p, x)) error stop
    if (.not. associated(a(2)%p, x)) error stop
    allocate(c(2))
    c = [t(w), t(w)]
    if (c(2)%p%i /= 1) error stop
end subroutine

subroutine check_array_assignment(x, y)
    type(s), target, intent(in) :: x, y
    type(t) :: a(2), b(2)
    type(t), allocatable :: c(:), d(:)
    a(1)%p => x
    a(2)%p => y
    b = a
    b(1)%p => y
    if (.not. associated(a(1)%p, x)) error stop
    if (b(2)%p%i /= 2) error stop
    allocate(c(2), d(2))
    c = a
    d = c
    d(2)%p => x
    if (.not. associated(c(2)%p, y)) error stop
    if (d(1)%p%i /= 1) error stop
end subroutine

subroutine check_allocate_source(x, y)
    type(s), target, intent(in) :: x, y
    type(t) :: a
    type(t), allocatable :: b, c(:)
    class(t), allocatable :: d
    a%p => x
    allocate(b, source=a)
    allocate(c(2), source=a)
    allocate(d, source=a)
    b%p => y
    c(1)%p => y
    d%p => y
    if (.not. associated(a%p, x)) error stop
    if (c(2)%p%i /= 1) error stop
    b = a
    if (b%p%i /= 1) error stop
    deallocate(b)
end subroutine

subroutine check_unlimited()
    integer, target :: k, m
    type(u) :: a, b
    k = 1
    m = 2
    a%q => k
    b = a
    if (.not. associated(b%q, k)) error stop
    b%q => m
    if (.not. associated(a%q, k)) error stop
    select type (q => a%q)
    type is (integer)
        if (q /= 1) error stop
    class default
        error stop
    end select
end subroutine

end module

program derived_types_221
use derived_types_221_mod
implicit none
type(s), target :: x, y
type(s2), target :: z
x%i = 1
y%i = 2
z%i = 3
z%j = 30
call check_scalar_copy(x, y)
call check_dynamic_type(z)
call check_array_constructor(x)
call check_array_assignment(x, y)
call check_allocate_source(x, y)
call check_unlimited()
if (x%i /= 1 .or. y%i /= 2) error stop
print *, "ok"
end program
