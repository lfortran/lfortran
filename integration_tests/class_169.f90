! Actuals that F2018 15.5.2.5 does not restrict: a nonpointer target for a
! polymorphic intent(in) pointer dummy, and associate names, which are never
! pointers. Matching pointer and allocatable actuals are also accepted.
module class_169_m
implicit none
type :: base_t
    integer :: a = 1
end type
type, extends(base_t) :: child_t
    integer :: b = 2
end type
contains
integer function class_ptr(w)
    class(base_t), pointer, intent(in) :: w
    if (.not. associated(w)) error stop 1
    class_ptr = w%a
end function
integer function type_ptr(w)
    type(base_t), pointer, intent(in) :: w
    if (.not. associated(w)) error stop 2
    type_ptr = w%a
end function
subroutine class_alloc(w)
    class(base_t), allocatable, intent(inout) :: w
    if (.not. allocated(w)) error stop 3
    w%a = w%a + 10
end subroutine
subroutine type_alloc(w)
    type(base_t), allocatable, intent(inout) :: w
    if (.not. allocated(w)) error stop 4
    w%a = w%a + 20
end subroutine
end module

program class_169
use class_169_m
implicit none
type(base_t), target :: bt
type(child_t), target :: ct
class(base_t), pointer :: cp
type(base_t), pointer :: tp
class(base_t), allocatable :: ca
type(base_t), allocatable :: ta

bt%a = 3
ct%a = 4
if (class_ptr(bt) /= 3) error stop 5
if (class_ptr(ct) /= 4) error stop 6
if (type_ptr(bt) /= 3) error stop 7

cp => ct
tp => bt
if (class_ptr(cp) /= 4) error stop 8
if (type_ptr(tp) /= 3) error stop 9

select type (q => cp)
type is (child_t)
    if (class_ptr(q) /= 4) error stop 10
class default
    error stop 11
end select

associate (r => tp)
    if (type_ptr(r) /= 3) error stop 12
    if (class_ptr(r) /= 3) error stop 13
end associate

allocate(child_t :: ca)
allocate(ta)
call class_alloc(ca)
call type_alloc(ta)
if (ca%a /= 11) error stop 14
if (ta%a /= 21) error stop 15
select type (ca)
type is (child_t)
    if (ca%b /= 2) error stop 16
class default
    error stop 17
end select
print *, "ok"
end program
