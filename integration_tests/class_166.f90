! Intrinsic assignment of a non-polymorphic array to an unallocated
! polymorphic allocatable array, or one of a different shape, with the
! LHS reallocated on assignment: the LHS takes the shape and the dynamic
! type of the expression (F2018 10.2.1.3). Also for a class(*) array and
! for a polymorphic array component.
module class_166_m
implicit none
type :: base_t
    integer :: a = 0
end type
type, extends(base_t) :: child_t
    integer :: b = 0
end type
type :: holder_t
    class(base_t), allocatable :: arr(:)
end type
contains
function mk(n) result(r)
    integer, intent(in) :: n
    type(child_t), allocatable :: r(:)
    allocate(r(0:n-1))
    r%a = 5; r%b = 7
end function
end module

program class_166
use class_166_m
implicit none
class(base_t), allocatable :: t(:)
type(child_t), allocatable :: c(:)
type(base_t) :: d(4)
class(*), allocatable :: u(:)
type(holder_t) :: h1, h2
type(holder_t), allocatable :: hs(:)
integer :: i

t = mk(3)
if (size(t) /= 3) error stop 1
if (lbound(t, 1) /= 1) error stop 2
select type (t)
type is (child_t)
    if (any(t%a /= 5)) error stop 3
    if (any(t%b /= 7)) error stop 4
class default
    error stop 5
end select

d%a = 3
t = d
if (size(t) /= 4) error stop 6
select type (t)
type is (base_t)
    if (any(t%a /= 3)) error stop 7
class default
    error stop 8
end select

deallocate(t)
allocate(c(3))
c%b = [(10*i, i = 1, 3)]
t = c
if (size(t) /= 3) error stop 9
select type (t)
type is (child_t)
    if (any(t%b /= [10, 20, 30])) error stop 10
class default
    error stop 11
end select

t = c([3, 1])
if (size(t) /= 2) error stop 12
select type (t)
type is (child_t)
    if (any(t%b /= [30, 10])) error stop 13
class default
    error stop 14
end select

u = [1, 2]
u = mk(3)
if (size(u) /= 3) error stop 15
select type (u)
type is (child_t)
    if (any(u%b /= 7)) error stop 16
class default
    error stop 17
end select
u = d
if (size(u) /= 4) error stop 18
select type (u)
type is (base_t)
    if (any(u%a /= 3)) error stop 19
class default
    error stop 20
end select
u = c(1:2)
if (size(u) /= 2) error stop 21
select type (u)
type is (child_t)
    if (any(u%b /= [10, 20])) error stop 22
class default
    error stop 23
end select

h1%arr = mk(2)
h2 = h1
h2%arr = d
if (size(h1%arr) /= 2 .or. size(h2%arr) /= 4) error stop 24
select type (p => h1%arr)
type is (child_t)
    if (any(p%b /= 7)) error stop 25
class default
    error stop 26
end select
select type (p => h2%arr)
type is (base_t)
    if (any(p%a /= 3)) error stop 27
class default
    error stop 28
end select
allocate(hs(2))
hs(2)%arr = c
select type (p => hs(2)%arr)
type is (child_t)
    if (any(p%b /= [10, 20, 30])) error stop 29
class default
    error stop 30
end select
print *, "ok"
end program
