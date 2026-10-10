! Intrinsic assignment of a non-polymorphic allocatable array function
! result to a polymorphic allocatable array: the LHS takes the shape,
! lower bound 1 and the dynamic type of the result.
module class_165_m
implicit none
type :: base_t
    integer :: a = 0
end type
type, extends(base_t) :: child_t
    integer :: b = 0
end type
contains
function mk(n) result(r)
    integer, intent(in) :: n
    type(child_t), allocatable :: r(:)
    allocate(r(0:n-1))
    r%a = 5; r%b = 7
end function
function mk_base(n) result(r)
    integer, intent(in) :: n
    type(base_t), allocatable :: r(:)
    allocate(r(n))
    r%a = 9
end function
end module

program class_165
use class_165_m
implicit none
class(base_t), allocatable :: t(:)

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

t = mk_base(2)
if (size(t) /= 2) error stop 6
select type (t)
type is (base_t)
    if (any(t%a /= 9)) error stop 7
class default
    error stop 8
end select

t = mk(4)
if (size(t) /= 4) error stop 9
select type (t)
type is (child_t)
    if (any(t%b /= 7)) error stop 10
class default
    error stop 11
end select
print *, "ok"
end program
